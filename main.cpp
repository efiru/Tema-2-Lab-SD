#include <chrono>
#include <iomanip>
#include <iostream>
#include <random>
#include <bits/stdc++.h>
using namespace std;
using Clock = chrono::high_resolution_clock;

template<typename T>
class SkewBinomialHeap {
public:

    struct Node {
        T key;
        int rank = 0;
        vector<Node*> children;
        explicit Node(const T& k) : key(k) {}
    };

private:

    vector<Node*> roots;

    Node* link2(Node* a, Node* b) {

        if (b->key < a->key)
            swap(a, b);
        a->children.push_back(b);
        a->rank++;
        return a;
    }

    Node* skewLink(Node* x, Node* a, Node* b) {

        if (a->key < x->key)
            swap(a, x);
        if (b->key < x->key)
            swap(b, x);

        x->children.push_back(a);
        x->children.push_back(b);
        x->rank++;
        return x;
    }

    vector<Node*> normalize(vector<Node*> ts) {

        vector<Node*> out;
        out.reserve(ts.size());
        Node* carry = NULL;

        for (int i = 0; i < ts.size(); ++i) {

            Node* curr = ts[i];

            if (!carry) {

                if (i + 1 < ts.size() && ts[i+1]->rank == curr->rank) {
                    carry = link2(curr, ts[++i]);
                } else {
                    out.push_back(curr);
                }

            } else {

                if ((carry->rank) < (curr->rank)) {

                    out.push_back(carry);

                    if (i + 1 < ts.size() && ts[i+1]->rank == curr->rank) {
                        carry = link2(curr, ts[++i]);
                    } else {
                        out.push_back(curr);
                        carry = NULL;
                    }
                } else if (carry->rank > curr->rank) {
                    out.push_back(curr);
                } else {
                    carry = link2(carry, curr);
                }
            }
        }

        if (carry)
            out.push_back(carry);

        return out;
    }

    vector<Node*> mergeRoots(const vector<Node*>& A, const vector<Node*>& B) {

        vector<Node*> merged;
        merged.reserve(A.size() + B.size());

        int i = 0, j = 0;

        while (i < A.size() || j < B.size()) {

            if (j == B.size() || (i < A.size() && (A[i]->rank) < (B[j]->rank))) {
                merged.push_back(A[i++]);
            } else {
                merged.push_back(B[j++]);
            }
        }
        return normalize(merged);
    }

    void destroyAll(Node* t) {

        if (!t)
            return;

        for (auto* c : t->children)
            destroyAll(c);

        delete t;
    }

public:

    SkewBinomialHeap() = default;
    ~SkewBinomialHeap() {

        for (auto* r : roots)
            destroyAll(r);
    }

    bool empty() const { return roots.empty(); }

    Node* insert(const T& key) {

        Node* single = new Node(key);

        if (roots.size() >= 2 && roots[0]->rank == 0 && roots[1]->rank == 0) {

            Node* a = roots[0];
            Node* b = roots[1];

            roots.erase(roots.begin());
            roots.erase(roots.begin());

            Node* merged = skewLink(single, a, b);

            roots = mergeRoots({ merged }, roots);
            return merged;

        } else {

            roots.insert(roots.begin(), single);
            return single;
        }
    }

    void merge(SkewBinomialHeap& other) {

        roots = mergeRoots(roots, other.roots);
        other.roots.clear();
    }

    T getMin() {

        if (roots.empty())
            return -1;

        T m = roots[0]->key;
        for (auto* r : roots)
            if (r->key < m)
                m = r->key;
        return m;
    }

    T extractMin() {

        if (roots.empty())
            return -1;

        int idx = 0;
        T m = roots[0]->key;

        for (int i = 1; i < roots.size(); i++) {

            if (roots[i]->key < m) {

                m = roots[i]->key;
                idx = i;
            }
        }

        Node* minNode = roots[idx];
        roots.erase(roots.begin() + idx);
        auto kids = minNode->children;
        delete minNode;

        sort(kids.begin(), kids.end(), [](Node* a, Node* b) { return (a->rank) < (b->rank); });

        SkewBinomialHeap tmp;
        tmp.roots = kids;
        merge(tmp);

        return m;
    }

    void test() {

        cout << roots.size();
    }
};

template<typename T>
class FibonacciHeap {
public:

    struct Node {

        T key;
        int degree = 0;
        bool marked = false;
        Node* parent = nullptr;
        Node* child = nullptr;
        Node* left = nullptr;
        Node* right = nullptr;
        Node(const T& k)
                : key(k), left(this), right(this) {}
    };

private:
    Node* minRoot = nullptr;
    int n = 0;

    void link(Node* y, Node* x) {

        y->left->right = y->right;
        y->right->left = y->left;

        y->parent = x;
        if (!x->child) {
            x->child = y;
            y->left = y->right = y;
        } else {

            y->right = x->child;
            y->left  = x->child->left;
            x->child->left->right = y;
            x->child->left = y;
        }
        x->degree++;
        y->marked = false;
    }

    void consolidate() {

        int D = int(floor(log2(n))) + 1;
        vector<Node*> A(D+1, NULL);

        vector<Node*> roots;

        if (minRoot) {

            Node* cur = minRoot;
            do {
                roots.push_back(cur);
                cur = cur->right;
            } while (cur != minRoot);
        }

        for (Node* w : roots) {

            Node* x = w;
            int d = x->degree;

            while (A[d]) {

                Node* y = A[d];

                if (y->key < x->key)
                    swap(x, y);
                link(y, x);

                A[d] = NULL;
                d++;
            }
            A[d] = x;
        }


        minRoot = NULL;

        for (auto* ptr : A) {

            if (!ptr) continue;
            ptr->left = ptr->right = ptr;

            if (!minRoot) {
                minRoot = ptr;
            } else {

                ptr->right = minRoot;
                ptr->left  = minRoot->left;
                minRoot->left->right = ptr;
                minRoot->left = ptr;
                if (ptr->key < minRoot->key)
                    minRoot = ptr;
            }
        }
    }

    void cut(Node* x, Node* y) {

        if (x->right == x) {
            y->child = NULL;
        } else {
            x->right->left = x->left;
            x->left->right = x->right;
            if (y->child == x) y->child = x->right;
        }
        y->degree--;

        x->left  = minRoot->left;
        x->right = minRoot;
        minRoot->left->right = x;
        minRoot->left = x;

        x->parent = NULL;
        x->marked = false;
    }

    void cascadingCut(Node* y) {

        Node* z = y->parent;
        if (z) {
            if (!y->marked) {
                y->marked = true;
            } else {
                cut(y, z);
                cascadingCut(z);
            }
        }
    }

    void destroyAll(Node* x) {

        if (!x) return;
        Node* start = x;
        Node* cur   = x;

        do {

            Node* next = cur->right;
            destroyAll(cur->child);
            delete cur;
            cur = next;
        } while (cur != start);
    }

public:
    FibonacciHeap() = default;

    ~FibonacciHeap() {
        destroyAll(minRoot);
    }

    bool empty() const { return minRoot == NULL; }

    Node* insert(const T& key) {

        Node* x = new Node(key);

        if (!minRoot) {
            minRoot = x;
        } else {

            x->right = minRoot;
            x->left  = minRoot->left;
            minRoot->left->right = x;
            minRoot->left = x;
            if (x->key < minRoot->key)
                minRoot = x;
        }
        n++;
        return x;
    }

    const T& get_min() {

        if (minRoot == NULL)
            return -1;
        return minRoot->key;
    }


    T extract_min() {

        if (minRoot == NULL)
            return -1;
        Node* z = minRoot;

        if (z->child) {
            std::vector<Node*> kids;
            Node* c = z->child;
            do {
                kids.push_back(c);
                c = c->right;
            } while (c != z->child);

            for (Node* k : kids) {

                k->parent = NULL;

                k->left  = minRoot->left;
                k->right = minRoot;
                minRoot->left->right = k;
                minRoot->left = k;
            }
        }

        z->left->right = z->right;
        z->right->left = z->left;

        if (z == z->right) {
            minRoot = NULL;
        } else {
            minRoot = z->right;
            consolidate();
        }

        T ret = z->key;
        delete z;
        n--;
        return ret;
    }

    void decrease_key(Node* x, const T& newKey) {

        x->key = newKey;
        Node* y = x->parent;
        if (y && x->key < y->key) {
            cut(x, y);
            cascadingCut(y);
        }
        if (x->key < minRoot->key)
            minRoot = x;
    }

    void delete_node(Node* x) {
        decrease_key(x, std::numeric_limits<T>::lowest());
        extract_min();
    }
};

const int NMAX = 128, QMAX = 1000000;

struct Node {
    int dimension, key;
    int index;
    Node *parent;
    Node *left, *right;
    Node *child;

    Node(int key = 0, int index = 1e9)
            : dimension(0), key(key), index(index), parent(nullptr), left(nullptr),
              right(nullptr), child(nullptr) {}

    bool operator<(const Node &other) const {
        return key < other.key || (key == other.key && index < other.index);
    }
};

class TwoThreeHeap {
public:
    void merge(Node *treeList) {
        Node *add = treeList;
        do {
            if (heaps[add->dimension] == nullptr) {
                heaps[add->dimension] = add;
                n |= 1 << add->dimension;
                add = nullptr;
            } else {
                int dim = add->dimension;
                mergeTrunks(&heaps[dim], &add);
                if (!heaps[dim]) n ^= 1 << dim;
            }
        } while (add);
    }

    int extractMin() {
        Node *best = nullptr;
        int minPos = 0;
        for (int i = 0; (1 << i) <= n; ++i)
            if (heaps[i] && (!best || *heaps[i] < *best)) {
                minPos = i;
                best = heaps[i];
            }

        n ^= 1 << minPos;
        heaps[minPos] = nullptr;
        int answer = best->key;

        while (best->child) {
            Node *child = best->child;
            Node *l = child->left, *r = child->right;
            child->right->left = l;
            child->left->right = r;
            best->child = (best->child == l ? nullptr : l);
            child->left = child->right = child->parent = nullptr;
            merge(child);
        }
        delete best;
        return answer;
    }

    void mergeWith(TwoThreeHeap &other) {
        for (int i = 0; (1 << i) <= other.n; ++i)
            if (other.heaps[i]) {
                other.heaps[i]->left = other.heaps[i]->right = other.heaps[i]->parent =
                        nullptr;
                merge(other.heaps[i]);
                other.heaps[i] = nullptr;
            }
        other.n = 0;
    }

    void printHeap(bool show) {
        if (!show) return;
        cout << "TwoThreeHeap Contents:\n";
        for (int i = 0; i < kMaxRank; ++i)
            if (heaps[i]) {
                cout << "Tree at dimension " << i << ":\n";
                printTree(heaps[i]);
            }
    }

    TwoThreeHeap() : n(0), heaps() {}

private:
    static const int kMaxRank = 30;
    int n;
    array<Node *, kMaxRank> heaps;

    void addChild(Node *parent, Node *child) {
        if (!parent || !child) return;
        if (parent->child) {
            child->left = parent->child;
            child->right = parent->child->right;
            parent->child->right->left = child;
            parent->child->right = child;
        } else {
            child->left = child->right = child;
        }
        child->parent = parent;
        parent->child = child;
    }

    void replaceNode(Node *oldNode, Node *newNode) {
        if (oldNode->left == oldNode) {
            newNode->left = newNode->right = newNode;
        } else {
            newNode->left = oldNode->left;
            newNode->right = oldNode->right;
            oldNode->left->right = newNode;
            oldNode->right->left = newNode;
        }
        newNode->parent = oldNode->parent;
        if (oldNode->parent && oldNode->parent->child == oldNode)
            oldNode->parent->child = newNode;
        oldNode->parent = nullptr;
        oldNode->left = oldNode->right = oldNode;
    }

    void mergeTrunks(Node **x, Node **y) {
        Node *parent = (**x < **y) ? *x : *y;
        Node *son = (parent == *x) ? *y : *x;

        Node *pChild = parent->child;
        if (pChild && pChild->dimension != son->dimension) pChild = nullptr;
        Node *sChild = son->child;
        if (sChild && sChild->dimension != son->dimension) sChild = nullptr;

        if (pChild && sChild) {
            replaceNode(pChild, son);
            pChild->left = pChild->right = pChild;
            ++parent->dimension;
            *x = pChild;
            *y = parent;
        } else if (pChild) {
            if (*pChild < *son)
                addChild(pChild, son);
            else {
                replaceNode(pChild, son);
                addChild(son, pChild);
            }
            ++parent->dimension;
            *x = nullptr;
            *y = parent;
        } else if (sChild) {
            addChild(parent, son);
            ++parent->dimension;
            *x = nullptr;
            *y = parent;
        } else {
            addChild(parent, son);
            *x = parent;
            *y = nullptr;
        }
    }

    void printTree(Node *node, int depth = 0) {
        if (!node) return;
        for (int i = 0; i < depth; ++i) cout << "  ";
        cout << "Node(key=" << node->key << ", dim=" << node->dimension << ")";
        if (node->child)
            cout << " -> [child=" << node->child->key << " dim=" << node->child->dimension << "]";
        cout << '\n';
        if (!node->child) return;
        Node *child = node->child;
        do {
            printTree(child, depth + 1);
            child = child->right;
        } while (child != node->child);
    }
};

Node *nodeOrder[QMAX];
int nodeCounter = 0;
TwoThreeHeap heaps[NMAX];

FibonacciHeap <int> f;

SkewBinomialHeap <int> s;

int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    using Clock = chrono::high_resolution_clock;

    mt19937_64 rng(12345);

    const int N_BULK    = 1000000;
    const int N_INTER   = 2000000;
    const int N_PART    = 2000000;
    const int N_HEAPS   = 2000;
    const int HEAP_SZ   = 2000;

    vector<int> asc(N_BULK), desc(N_BULK), rnd(N_BULK);
    iota(asc.begin(), asc.end(), 0);
    desc = asc; reverse(desc.begin(), desc.end());
    rnd  = asc; shuffle(rnd.begin(), rnd.end(), rng);

    auto print_time = [&](string label, Clock::time_point t0, Clock::time_point t1){
        double s = chrono::duration<double>(t1 - t0).count();
        cout << setw(12) << label << " : " << fixed << setprecision(3) << s << " s\n";
    };

    auto run_bulk = [&](string name, const vector<int>& data){
        cout << "\n=== Bulk " << name << " (" << data.size() << ") ===\n";

        // SkewBinomialHeap
        {
            SkewBinomialHeap<int> H;
            auto t0 = Clock::now();
            for (int x : data) H.insert(x);
            auto t1 = Clock::now();
            for (size_t i = 0; i < data.size(); ++i) H.extractMin();
            auto t2 = Clock::now();

            print_time("Skew ins", t0, t1);
            print_time("Skew ext", t1, t2);
        }

        // FibonacciHeap
        {
            FibonacciHeap<int> H;
            auto t0 = Clock::now();
            for (int x : data) H.insert(x);
            auto t1 = Clock::now();
            for (size_t i = 0; i < data.size(); ++i) H.extract_min();
            auto t2 = Clock::now();

            print_time("Fib  ins", t0, t1);
            print_time("Fib  ext", t1, t2);
        }

        // TwoThreeHeap
        {
            TwoThreeHeap H;
            int idx = 0;
            auto t0 = Clock::now();
            for (int x : data) {
                Node* nd = new Node(x, idx++);
                nd->left = nd->right = nd->parent = nd->child = nullptr;
                H.merge(nd);
            }
            auto t1 = Clock::now();
            for (size_t i = 0; i < data.size(); ++i) H.extractMin();
            auto t2 = Clock::now();

            print_time("2-3  ins", t0, t1);
            print_time("2-3  ext", t1, t2);
        }
    };

    run_bulk("Ascending",  asc);
    run_bulk("Descending", desc);
    run_bulk("Random",     rnd);

    {
        cout << "\n=== Interleaved Random (" << N_INTER << ") ===\n";
        vector<int> seq(N_INTER);
        for (auto &v : seq) v = int(rng());

        // Skew
        {
            SkewBinomialHeap<int> H;
            int inserted = 0, extracted = 0, size = 0;
            auto t0 = Clock::now();
            while (inserted < N_INTER || extracted < N_INTER) {
                if (inserted < N_INTER && ((rng() & 1) || size == 0)) {
                    H.insert(seq[inserted++]);
                    ++size;
                } else {
                    H.extractMin();
                    --size;
                    ++extracted;
                }
            }
            auto t1 = Clock::now();
            print_time("Skew inter", t0, t1);
        }

        // Fibonacci
        {
            FibonacciHeap<int> H;
            int inserted = 0, extracted = 0, size = 0;
            auto t0 = Clock::now();
            while (inserted < N_INTER || extracted < N_INTER) {
                if (inserted < N_INTER && ((rng() & 1) || size == 0)) {
                    H.insert(seq[inserted++]);
                    ++size;
                } else {
                    H.extract_min();
                    --size;
                    ++extracted;
                }
            }
            auto t1 = Clock::now();
            print_time("Fib  inter", t0, t1);
        }

        // TwoThreeHeap
        {
            TwoThreeHeap H;
            int inserted = 0, extracted = 0, size = 0, idx = 0;
            auto t0 = Clock::now();
            while (inserted < N_INTER || extracted < N_INTER) {
                if (inserted < N_INTER && ((rng() & 1) || size == 0)) {
                    Node* nd = new Node(seq[inserted], idx++);
                    nd->left = nd->right = nd->parent = nd->child = nullptr;
                    H.merge(nd);
                    ++inserted; ++size;
                } else {
                    H.extractMin();
                    --size; ++extracted;
                }
            }
            auto t1 = Clock::now();
            print_time("2-3  inter", t0, t1);
        }
    }

    {
        cout << "\n=== Partial Workload ===\n";
        vector<int> seq(N_PART);
        for (auto &v : seq)
            v = int(rng());

        auto run_phase = [&](auto &H, auto extract_fn, string name = ""){
            int i1 = 0, e1 = 0, i2 = 0, e2 = 0;
            int ins1 = N_PART, ext1 = N_PART/2, ins2 = N_PART/2, ext2 = N_PART;
            int idx = 0, size = 0;
            // phase1 insert
            auto t0 = Clock::now();
            while (i1 < ins1) {
                H.insert(seq[i1++]); ++size;
            }
            auto t1 = Clock::now();
            // phase1 extract
            while (e1 < ext1) {
                (H.*extract_fn)(); --size; ++e1;
            }
            auto t2 = Clock::now();
            // phase2 insert
            while (i2 < ins2) {
                H.insert(seq[i2++]+1'000'000); ++size;
            }
            auto t3 = Clock::now();
            // phase2 extract all
            while (e2 < ext2) {
                (H.*extract_fn)(); --size; ++e2;
            }
            auto t4 = Clock::now();

            print_time(name + " ins1", t0, t1);
            print_time(name + " ext1", t1, t2);
            print_time(name + " ins2", t2, t3);
            print_time(name + " ext2", t3, t4);
        };

        // Skew
        {
            SkewBinomialHeap<int> H;
            run_phase(H, &SkewBinomialHeap<int>::extractMin, "Skew");
        }
        // Fibonacci
        {
            FibonacciHeap<int> H;
            run_phase(H, &FibonacciHeap<int>::extract_min, "Fib");
        }
        // TwoThreeHeap
        {
            struct Wrap {
                TwoThreeHeap& H; int& idx;
                Wrap(TwoThreeHeap& h,int&i):H(h),idx(i){}
                void insert(int x){
                    Node* nd=new Node(x,idx++);
                    nd->left = nd->right = nd->parent = nd->child = nullptr;
                    H.merge(nd);
                }
                int extract() { return H.extractMin(); }
            };
            TwoThreeHeap raw;
            int idx = 0;
            Wrap W(raw,idx);
            run_phase(W, &Wrap::extract, "2-3");
        }
    }

    {
        cout << "\n=== Merge Chains ===\n";
        // Skew
        {
            vector<SkewBinomialHeap<int>> v(N_HEAPS);
            for (auto &h : v) for (int i=0;i<HEAP_SZ;++i) h.insert(int(rng()));
            auto t0 = Clock::now();
            for (int i = 1; i < N_HEAPS; ++i) v[0].merge(v[i]);
            auto t1 = Clock::now();
            while (!v[0].empty()) v[0].extractMin();
            auto t2 = Clock::now();
            print_time("Skew merge", t0, t1);
            print_time("Skew ext  ", t1, t2);
        }
        // Fibonacci
        {
            vector<FibonacciHeap<int>> v(N_HEAPS);
            vector<vector<int>> vals(N_HEAPS, vector<int>(HEAP_SZ));
            for (int i=0;i<N_HEAPS;++i){
                for (int j = 0; j < HEAP_SZ; ++j){
                    int x=int(rng());
                    vals[i][j]=x;
                    v[i].insert(x);
                }
            }
            auto t0 = Clock::now();
            for (int i = 1; i < N_HEAPS; ++i)
                for (int x: vals[i])
                    v[0].insert(x);
            auto t1 = Clock::now();
            while (!v[0].empty())
                v[0].extract_min();
            auto t2 = Clock::now();
            print_time("Fib  merge", t0, t1);
            print_time("Fib  ext  ", t1, t2);
        }
        // TwoThreeHeap
        {
            vector<TwoThreeHeap> v(N_HEAPS);
            int idx = 0;
            for (auto &h : v)
                for (int i=0;i<HEAP_SZ;++i){
                    int x = int(rng());
                    Node* nd = new Node(x,idx++);
                    nd->left = nd->right = nd->parent = nd->child = nullptr;
                    h.merge(nd);
                }
            auto t0 = Clock::now();
            for (int i = 1; i < N_HEAPS; ++i)
                v[0].mergeWith(v[i]);
            auto t1 = Clock::now();
            while (true) {
                static int total = N_HEAPS*HEAP_SZ;
                for (int k = 0; k < total; ++k)
                    v[0].extractMin();
                break;
            }
            auto t2 = Clock::now();
            print_time("2-3  merge", t0, t1);
            print_time("2-3  ext  ", t1, t2);
        }
    }

    return 0;
}
