#include <iostream>

class Node {
  int *keys;
  int block_limit;
  Node **children;
  int n;
  bool is_leaf;

public:
  Node(int block_limit, bool if_node_is_leaf);

  void insertNonFull(int k);
  void splitChild(int i, Node *y);
  void traverse();

  Node *search(int k);
  int* get_keys(Node* node);

  friend class BTree;
};

Node::Node(int limit, bool if_node_is_leaf) {
  block_limit = limit;
  is_leaf = if_node_is_leaf;

  keys = new int[2 * block_limit - 1];
  children = new Node *[2 * block_limit];

  n = 0;
}

int *Node::get_keys(Node *node) {
  return node->keys;
}

void Node::traverse() {
  int i;
  for (i = 0; i < n; i++) {
    if (!is_leaf)
      children[i]->traverse();
    std::cout << " " << keys[i];
  }

  if (!is_leaf)
    children[i]->traverse();
}

Node *Node::search(int k) {
  int i = 0;
  while (i < n && k > keys[i])
    i++;

  if (keys[i] == k)
    return this;

  if (is_leaf)
    return NULL;

  return children[i]->search(k);
}

class BTree {
  Node *root;
  int block_limit;

public:
  BTree(int temp) {
    root = NULL;
    block_limit = temp;
  }

  void traverse() {
    if (root != NULL)
      root->traverse();
  }

  Node *search(int k) { return (root == NULL) ? NULL : root->search(k); }

  void insert(int k);
};

void BTree::insert(int k) {
  if (root == NULL) {
    root = new Node(block_limit, true);
    root->keys[0] = k;
    root->n = 1;
  } else {
    if (root->n == 2 * block_limit - 1) {
      Node *s = new Node(block_limit, false);

      s->children[0] = root;

      s->splitChild(0, root);

      int i = 0;
      if (s->keys[0] < k)
        i++;
      s->children[i]->insertNonFull(k);

      root = s;
    } else
      root->insertNonFull(k);
  }
}

void Node::insertNonFull(int k) {
  int i = n - 1;

  if (is_leaf) {
    while (i >= 0 && keys[i] > k) {
      keys[i + 1] = keys[i];
      i--;
    }

    keys[i + 1] = k;
    n = n + 1;
  } else {
    while (i >= 0 && keys[i] > k)
      i--;

    if (children[i + 1]->n == 2 * block_limit - 1) {
      splitChild(i + 1, children[i + 1]);

      if (keys[i + 1] < k)
        i++;
    }
    children[i + 1]->insertNonFull(k);
  }
}

void Node::splitChild(int i, Node *y) {
  Node *z = new Node(y->block_limit, y->is_leaf);
  z->n = block_limit - 1;

  for (int j = 0; j < block_limit - 1; j++)
    z->keys[j] = y->keys[j + block_limit];

  if (!y->is_leaf) {
    for (int j = 0; j < block_limit; j++)
      z->children[j] = y->children[j + block_limit];
  }

  y->n = block_limit - 1;
  for (int j = n; j >= i + 1; j--)
    children[j + 1] = children[j];

  children[i + 1] = z;

  for (int j = n - 1; j >= i; j--)
    keys[j + 1] = keys[j];

  keys[i] = y->keys[block_limit - 1];
  n = n + 1;
}
extern "C" {
  BTree *CreateBTree(int level) { return new BTree(level); }
  void insert(BTree* tree, int value){
    (*tree).insert(value);
    return;
  }
  int* search(BTree* tree, int value){
    Node* node = (*tree).search(value);
    return node->get_keys(node);
  }
}
