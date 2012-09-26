#include <set>
#include <queue>
#include <iostream>
using namespace std;

int main(int argc, char* argv[]) {
  set<int> s;
  priority_queue<int, vector<int>, greater<int> > q;
  while (--argc) {
    int a = std::atoi(*++argv);
    s.insert(a);
    q.push(a);
  }
  while (!s.empty()) {
    cout << *s.begin() << " ";
    s.erase(s.begin());
  }
  cout << endl;
  while (!q.empty()) {
    cout << q.top() << " ";
    q.pop();
  }
  cout << endl;
  return 0;
}


