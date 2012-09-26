Node* build_tree(int* preorder, int* inorder, int sz) {
 if (sz == 0) return 0;
 if (sz == 1) return new Node(*preorder);
 int* root = lower_bound(inorder, inorder + sz, *preorder);
 int root_i = root - inorder;
 Node* node = build_tree(root, preorder, 1);
 node->left = build_tree(preorder + 1, inorder, root_i);
 node->right = build_tree(preorder + root_i + 1, root + 1, sz - root_i - 1);
 return node;
}

Inorder sequence: D B E A F C
Preorder sequence: A B D E C F

In a Preorder sequence, leftmost element is the root of the tree. So
we know ‘A’ is root for given sequences. By searching ‘A’ in Inorder
sequence, we can find out all elements on left side of ‘A’ are in left
subtree and elements on right are in right subtree. So we know below
structure now.

                A
              /   \
            /       \
          D B E     F C
We recursively follow above steps and get the following tree.

        A
      /   \
    /       \
   B         C
  / \        /
 /     \    /
D       E  F

