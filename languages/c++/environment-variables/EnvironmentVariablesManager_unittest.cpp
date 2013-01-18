#include <iostream>
#include <string>
#include <vector>
#include <cstdlib>
#include <cstring>
#include <cassert>

#ifdef WINDOWS
#include <windows.h>
#else
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#endif

#include "EnvironmentVariablesManager.h"

void Test_EnvironmentVariablesManager_get_put() {
  EnvironmentVariablesManager env;
  assert(std::string("") == env.get("_a_unique_variable_"));
  env.put("_a_unique_variable_", "b");
  assert(std::string("b") == env.get("_a_unique_variable_"));
  env.put("_a_unique_variable_", "abc");
  assert(std::string("abc") == env.get("_a_unique_variable_"));
  env.put("_a_unique_variable_", "");
  assert(std::string("") == env.get("_a_unique_variable_"));
}

namespace {
std::string ReadEnvironmentVariableViaShell(const std::string& name) {
#ifdef WINDOWS
  const std::string shell =
    EnvironmentVariablesManager::GetOSVariable("ComSpec");
  assert(!shell.empty());
  const std::string cmd = shell + " /c echo %" + name + "%";
  FILE* const f = _popen(cmd.c_str(), "rb");
#else
  const std::string cmd = "echo $" + name;
  FILE* const f = popen(cmd.c_str(), "r");
#endif
  assert(f != NULL);
  std::vector<char> line(1024, 0);
  size_t read = 0;
  while (!::feof(f) && read < line.size()) {
    const size_t sz = ::fread(&line[read], 1, line.size() - read, f);
    read += sz;
  }
#ifdef WINDOWS
  ::_pclose(f);
#else
  ::pclose(f);
#endif
  line.resize(read);
  std::string trimmed(read, '\0');
  std::copy(line.begin(), line.end(), trimmed.begin());
  return trimmed.substr(0, trimmed.find_last_not_of("\r\n") + 1);
}
}

void Test_EnvironmentVariablesManager_put_is_propagated_to_child_process() {
  EnvironmentVariablesManager env;
#ifdef WINDOWS
  const std::string empty = "%__unique_%";
#else
  const std::string empty = "";
#endif
  assert(empty == ReadEnvironmentVariableViaShell("__unique_"));
  env.put("__unique_", "b");
  assert(std::string("b") == ReadEnvironmentVariableViaShell("__unique_"));
  env.put("__unique_", "");
  assert(empty == ReadEnvironmentVariableViaShell("__unique_"));
}

void Test_EnvironmentVariablesManager_must_take_a_copy() {
  EnvironmentVariablesManager env;
  char var[] = "12345678";
  env.put("var", var);
  assert(env.get("var") == std::string("12345678"));
  std::strcpy(var, "abc");
  assert(env.get("var") == std::string("12345678"));
}

void Test_EnvironmentVariablesManager_del() {
  EnvironmentVariablesManager env;
  env.put("variable_to_delete", "123");
  assert(std::string("123") == env.get("variable_to_delete"));
  env.del("variable_to_delete");
  assert(env.get("variable_to_delete").empty() == true);
}

void Test_EnvironmentVariablesManager_IsOSVariableSet_set_and_unset() {
  EnvironmentVariablesManager env;
  env.put("a", "value");
  assert(EnvironmentVariablesManager::IsOSVariableSet("a") == true);
  env.put("a", "");
  assert(EnvironmentVariablesManager::IsOSVariableSet("a") == false);
}

void Test_EnvironmentVariablesManager_GetOSVariable() {
  const std::string unique_name = "EnvironmentVariablesManager_GetOSVariable";
  assert(EnvironmentVariablesManager::GetOSVariable(unique_name.c_str())
                                                    .empty());
  const std::string unique_name_pair = unique_name + "=12345678";
  char var[1024];
  unique_name_pair.copy(var, sizeof(var));
  var[unique_name_pair.length()] = '\0';
  ::putenv(var);
  assert(EnvironmentVariablesManager::GetOSVariable(unique_name.c_str())
                                                    == "12345678");
}

void Test_EnvironmentVariablesManager_PutOSVariable() {
  const std::string unique_name = "EnvironmentVariablesManager_PutOSVariable";
  const char* before = ::getenv(unique_name.c_str());
  if (before != NULL)
    assert(std::string(before).empty());

  const std::string unique_name_pair = unique_name + "=12345678";
  char var[1024];
  unique_name_pair.copy(var, sizeof(var));
  var[unique_name_pair.length()] = '\0';

  EnvironmentVariablesManager::PutOSVariable(var);
  const char* after = ::getenv(unique_name.c_str());
  assert(after != NULL);
  assert(std::string(after) == "12345678");
}

int run_tests(int argc, const char* const argv[]) {
  Test_EnvironmentVariablesManager_GetOSVariable();
  Test_EnvironmentVariablesManager_PutOSVariable();
  Test_EnvironmentVariablesManager_get_put();
  Test_EnvironmentVariablesManager_put_is_propagated_to_child_process();
  Test_EnvironmentVariablesManager_must_take_a_copy();
  Test_EnvironmentVariablesManager_del();
  Test_EnvironmentVariablesManager_IsOSVariableSet_set_and_unset();
  return 0;
}

int main(int argc, const char* const argv[]) {
  run_tests(argc, argv);
  std::cout << "All tests pass." << std::endl;
}
