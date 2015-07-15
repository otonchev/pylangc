# Simple Python-based Static Code Analyzer for C programs based on
# GLib/GStreamer, which takes advantage of the GObject-Introspection
# annotations built on top of GTK-Doc comment blocks. Note that this
# tool is proof-of-concept only.
#
# Copyright, 2015, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("../..")
from pylangparser import *

import c_grammar
import gtk_doc_grammar

# our C source code
source = r"""

int p (GstElement *el, gboolean doit)
{
  GstElement *parent;
  int j;
  int arr[];
  int arr2[5];

  parent = gst_element_get_parent (el);

  {
    GstPad *pad;

    pad = gst_element_get_static_pad (parent);

    do_something_with_pad (pad);

    //if (doit)
    //  return 1;

    if (doit)
      gst_object_unref (pad);
    else
      //gst_object_unref (pad);
      printf("hello world!");

    //gst_object_unref (pad);
  }

  gst_object_unref (parent);

  return 0;
}

void b()
{
  GstElement *parent;

  parent = gst_element_get_parent (el);
  do_something_with_pad (pad);

  if (FALSE)
    gst_object_unref (parent);

  gst_element_get_parent (el);
}

"""

# and a list of GTK-Doc documented functions
gtk_doc = r"""

/**
 * gst_element_get_parent:
 * @elem: a #GstElement to get the parent of.
 *
 * Get the parent of an element.
 *
 * Returns: (transfer full): the parent of an element.
 */

/**
 * gst_element_get_static_pad:
 * @element: a #GstElement to find a static pad of.
 * @name: the name of the static #GstPad to retrieve.
 *
 * Retrieves a pad from @element by name. This version only retrieves
 * already-existing (i.e. 'static') pads.
 *
 * Returns: (transfer full) (nullable): the requested #GstPad if
 *     found, otherwise %NULL.  unref after usage.
 *
 * MT safe.
 */

/**
 * gst_object_unref:
 * @object: (type Gst.Object): a #GstObject to unreference
 *
 * Decrements the reference count on @object.  If reference count hits
 * zero, destroy @object. This function does not take the lock
 * on @object as it relies on atomic refcounting.
 *
 * The unref method should never be called with the LOCK held since
 * this might deadlock the dispose function.
 */

"""

# obtain a list of all tokens present in the C source
c_lexer = Lexer(c_grammar.TOKENS)
c_tokens = c_lexer.parseTokens(source)
print("-------\nTOKENS (C source):\n-------")
print(c_tokens)

# generate AST for the C program
c_result = c_grammar.translation_unit(c_tokens, 0)
print("----\nAST:\n----")
print(c_result)
print("--------------\nHumanized AST (C source):\n--------------")
c_result.pretty_print()

# obtain a list of all tokens present in the doc
gtk_lexer = Lexer(gtk_doc_grammar.TOKENS)
gtk_tokens = gtk_lexer.parseTokens(gtk_doc, False)
print("-------\nTOKENS (GTK-Doc):\n-------")
print(gtk_tokens)

# generate AST for the doc
gtk_result = gtk_doc_grammar.parser(gtk_tokens, 0)
print("--------------\nHumanized AST (GTK-Doc):\n--------------")
gtk_result.pretty_print()


def parse_gtk_doc(ast):
    """
    Returns dictionary where key is a func name and value is a tuple of
    this kind:
    (<True or False>, (<True or False>,)). True indicates transfer-full.
           ^                       ^
           |                       |
         return               list of args
    """
    result = {}

    for function_block in ast:

        ret_name = None
        ret_ret = False
        ret_args = ()

        for function_desc in function_block:

            if function_desc.check_parser(gtk_doc_grammar.func_name):
                # function name
                ret_name = function_desc.get_token()

            if function_desc.check_parser(gtk_doc_grammar.arg):
                # unref should always be considered transfer-full
                if ret_name.endswith("_unref"):
                    ret_args = ret_args + (True,)
                    continue
                full = False
                index = 0
                for func_arg in function_desc:
                    if index > 1:
                        # annotations
                        if func_arg.get_token() == "(transfer full)":
                            full = True
                    index += 1
                ret_args = ret_args + (full,)

            if function_desc.check_parser(gtk_doc_grammar.returns):
                # return
                full = False
                index = 0
                for func_ret in function_desc:
                    if index > 0:
                        if func_ret.get_token() == "(transfer full)":
                            full = True
                    index += 1
                ret_ret = full

        result[ret_name] = (ret_ret, ret_args)

    return result

def perform_call_search(group, level, gtk_table, allocations):
    """
    Recursively inspect function definition for variable allocations/frees.
    """
    for sub_group in group:

        var_name = None

        # modify expression
        if sub_group.check_parser(c_grammar.modify_expression):
            # modify expression with a function call, we should check whether
            # this function allocates memory
            if sub_group[3].check_parser(c_grammar.call_expression):
                # var = func (args)
                #  ^  ^      ^
                #  1  2      3

                # remember variable name and proceed with the function call
                var_name = sub_group[1].get_token()
                sub_group = sub_group[3]

        # function call, does it allocate/free memory?
        if sub_group.check_parser(c_grammar.call_expression):
            # current sub_group is a call expression
            # func (args)
            #   ^    ^
            #   1    2
            func_name = sub_group[1].get_token()
            args = None

            print("calling function %s" % func_name)

            try:
                # function found in gtk doc
                args = gtk_table[func_name]
            except KeyError:
                # function not found in gtk doc
                continue

            (gtk_ret, gtk_input_args) = args
            if gtk_ret:
                if var_name is not None:
                    # allocating variable
                    print("allocating variable: %s" % var_name)
                    allocations.append(var_name)
                else:
                    print("leaking memory")
                    allocations.append("<NO_NAME>")

            func_args = sub_group[2]

            index = 0
            for arg in func_args:
                if gtk_input_args[index]:
                    arg_name = arg.get_token()
                    # freeing variable
                    try:
                        i = allocations.index(arg_name)
                        print("freeing variable: %s" % arg_name)
                        allocations.pop(i)
                    except ValueError:
                        print("warning, variable '%s' not previously "
                            "allocated, allocations: %s" % (arg_name,
                                                             allocations))
                index += 1

        # return statement
        elif sub_group.check_parser(c_grammar.stop_statement):
            if len(allocations) > 0:
                print("WARNING: function return, non freed allocations: %s" %
                    allocations)
            return False

        # if statement
        elif sub_group.check_parser(c_grammar.if_statement):
            # perform deep search
            # sub group 1 - if, 2 - cond, 3 - statement, 4 - else, 5 - statement
            # we are interested in statements

            new_allocations1 = list(allocations)
            new_allocations2 = list(allocations)

            # check first branch of if..else
            ret = perform_call_search([sub_group[3],], level + 1, gtk_table,
                new_allocations1)
            if not ret:
                # branch ended up in return, function is exitting
                allocations[:] = []
                allocations = allocations + new_allocations1
                return ret

            # check second branch of if..else
            if sub_group[4] is not None:
                ret = perform_call_search([sub_group[5],], level + 1, gtk_table,
                    new_allocations2)
                if not ret:
                    # branch ended up in return, function is exitting
                    allocations[:] = []
                    allocations = allocations + new_allocations2
                    return ret

            # choose longer list assuming unref was missed
            allocations[:] = []
            if len(new_allocations1) > len(new_allocations2):
                allocations.extend(new_allocations1)
            else:
                allocations.extend(new_allocations2)

        # new block {}
        elif sub_group.check_parser(c_grammar.compound_statement):
            # perform deep search
            ret = perform_call_search(sub_group, level + 1, gtk_table, allocations)
            if not ret:
                return ret

        # something else
        else:
            # perform deep search
            ret = perform_call_search(sub_group, level, gtk_table, allocations)
            if not ret:
                return ret

    return True

gtk_table = parse_gtk_doc(gtk_result)
print("\nparsed gtk: %s" % gtk_table)

# iterate AST for the C source
for func_def in c_result:
    # found function definition, check it out
    if func_def.check_parser(c_grammar.function_definition):
        print("\nINFO: found function definition:")
        func_def.pretty_print()
        print("\n\n")
        allocations = []
        perform_call_search(func_def, 0, gtk_table, allocations)
        if len(allocations) > 0:
            print("WARNING: function returned, non freed allocations: %s" %
                allocations)
        else:
            print("INFO: no potential leaks found")
