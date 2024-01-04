#pragma once

#ifdef PERPLEXUTILITIES_EXPORTS
#define PERPLEXUTILITIES_API __declspec(dllexport)
#else
#define PERPLEXUTILITIES_API __declspec(dllimport)
#endif

#define Version "1.16.1"

/* Type representing a node in the B+ tree.
 * This type is general enough to serve for both
 * the leaf and the internal node.
 * The heart of the node is the array
 * of keys and the array of corresponding
 * pointers.  The relation between keys
 * and pointers differs between leaves and
 * internal nodes.  In a leaf, the index
 * of each key equals the index of its corresponding
 * pointer, with a maximum of order - 1 key-pointer
 * pairs.  The last pointer points to the
 * leaf to the right (or NULL in the case
 * of the rightmost leaf).
 * In an internal node, the first pointer
 * refers to lower nodes with keys less than
 * the smallest key in the keys array.  Then,
 * with indices i starting at 0, the pointer
 * at i + 1 points to the subtree with keys
 * greater than or equal to the key in this
 * node at index i.
 * The num_keys field is used to keep
 * track of the number of valid keys.
 * In an internal node, the number of valid
 * pointers is always num_keys + 1.
 * In a leaf, the number of valid pointers
 * to data is always num_keys.  The
 * last leaf pointer points to the next leaf.
 */
typedef struct node {
	void** pointers;
	int* keys;
	struct node* parent;
	bool is_leaf;
	int num_keys;
	struct node* next;
} node;

extern "C" PERPLEXUTILITIES_API void print_leaves(node* const);
extern "C" PERPLEXUTILITIES_API int find_and_get_value(node* const, int, bool);