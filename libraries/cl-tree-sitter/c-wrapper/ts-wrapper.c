/*
 * Tree-sitter CFFI wrapper
 * Provides out-pointer versions of functions that return structs by value
 */

#include <tree_sitter/api.h>
#include <string.h>

/* Tree operations */
void ts_tree_root_node_out(TSNode *out, const TSTree *tree) {
    *out = ts_tree_root_node(tree);
}

/* Node operations */
void ts_node_child_out(TSNode *out, TSNode node, uint32_t index) {
    *out = ts_node_child(node, index);
}

void ts_node_named_child_out(TSNode *out, TSNode node, uint32_t index) {
    *out = ts_node_named_child(node, index);
}

void ts_node_parent_out(TSNode *out, TSNode node) {
    *out = ts_node_parent(node);
}

void ts_node_next_sibling_out(TSNode *out, TSNode node) {
    *out = ts_node_next_sibling(node);
}

void ts_node_prev_sibling_out(TSNode *out, TSNode node) {
    *out = ts_node_prev_sibling(node);
}

void ts_node_next_named_sibling_out(TSNode *out, TSNode node) {
    *out = ts_node_next_named_sibling(node);
}

void ts_node_prev_named_sibling_out(TSNode *out, TSNode node) {
    *out = ts_node_prev_named_sibling(node);
}

/* Point operations */
void ts_node_start_point_out(TSPoint *out, TSNode node) {
    *out = ts_node_start_point(node);
}

void ts_node_end_point_out(TSPoint *out, TSNode node) {
    *out = ts_node_end_point(node);
}

/* Tree cursor operations */
void ts_tree_cursor_new_out(TSTreeCursor *out, TSNode node) {
    *out = ts_tree_cursor_new(node);
}

void ts_tree_cursor_current_node_out(TSNode *out, const TSTreeCursor *cursor) {
    *out = ts_tree_cursor_current_node(cursor);
}

/* Wrapper functions that take node pointer instead of by-value */
const char *ts_node_type_ptr(const TSNode *node) {
    return ts_node_type(*node);
}

uint16_t ts_node_symbol_ptr(const TSNode *node) {
    return ts_node_symbol(*node);
}

uint32_t ts_node_start_byte_ptr(const TSNode *node) {
    return ts_node_start_byte(*node);
}

uint32_t ts_node_end_byte_ptr(const TSNode *node) {
    return ts_node_end_byte(*node);
}

uint32_t ts_node_child_count_ptr(const TSNode *node) {
    return ts_node_child_count(*node);
}

uint32_t ts_node_named_child_count_ptr(const TSNode *node) {
    return ts_node_named_child_count(*node);
}

bool ts_node_is_null_ptr(const TSNode *node) {
    return ts_node_is_null(*node);
}

bool ts_node_is_named_ptr(const TSNode *node) {
    return ts_node_is_named(*node);
}

bool ts_node_is_missing_ptr(const TSNode *node) {
    return ts_node_is_missing(*node);
}

bool ts_node_is_extra_ptr(const TSNode *node) {
    return ts_node_is_extra(*node);
}

bool ts_node_has_error_ptr(const TSNode *node) {
    return ts_node_has_error(*node);
}

char *ts_node_string_ptr(const TSNode *node) {
    return ts_node_string(*node);
}

void ts_node_start_point_ptr(TSPoint *out, const TSNode *node) {
    *out = ts_node_start_point(*node);
}

void ts_node_end_point_ptr(TSPoint *out, const TSNode *node) {
    *out = ts_node_end_point(*node);
}

void ts_node_child_ptr(TSNode *out, const TSNode *node, uint32_t index) {
    *out = ts_node_child(*node, index);
}

void ts_node_named_child_ptr(TSNode *out, const TSNode *node, uint32_t index) {
    *out = ts_node_named_child(*node, index);
}

void ts_node_parent_ptr(TSNode *out, const TSNode *node) {
    *out = ts_node_parent(*node);
}

void ts_node_next_sibling_ptr(TSNode *out, const TSNode *node) {
    *out = ts_node_next_sibling(*node);
}

void ts_node_prev_sibling_ptr(TSNode *out, const TSNode *node) {
    *out = ts_node_prev_sibling(*node);
}

void ts_node_next_named_sibling_ptr(TSNode *out, const TSNode *node) {
    *out = ts_node_next_named_sibling(*node);
}

void ts_node_prev_named_sibling_ptr(TSNode *out, const TSNode *node) {
    *out = ts_node_prev_named_sibling(*node);
}

/* Query cursor operations with pointer-based node */
void ts_query_cursor_exec_ptr(TSQueryCursor *cursor, const TSQuery *query, const TSNode *node) {
    ts_query_cursor_exec(cursor, query, *node);
}
