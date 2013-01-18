#include "EnvironmentVariablesManager.h"

#ifdef WINDOWS
#include <windows.h>
#else
#include <unistd.h>
#endif

#include <vector>
#include <map>
#include <string>
#include <iterator>
#include <algorithm>

#include <cassert>

void EnvironmentVariablesManager::put(const std::string& name,
                                      const std::string& value) {
  const VariableContainer pair = PairToContainer(name, value);
  const std::pair<Variables::iterator, bool> inserted =
    vars_.insert(std::make_pair(name, pair));
  if (!inserted.second)
    inserted.first->second = pair;
  char* data = &(inserted.first->second[0]);
  PutOSVariable(data);
}

std::string EnvironmentVariablesManager::get(const std::string& name) const {
  return GetOSVariable(name.c_str());
}

void EnvironmentVariablesManager::del(const std::string& name) {
  put(name, "");
}

void EnvironmentVariablesManager::PutOSVariable(char* value) {
  ::putenv(value);
}

std::string EnvironmentVariablesManager::GetOSVariable(const char* name) {
#ifdef WINDOWS
  size_t sz = 0;
  assert(getenv_s(&sz, NULL, 0, name) == 0);
  if (sz == 0) return std::string();
  std::vector<char> value(sz + 1);
  assert(getenv_s(&sz, &value[0], sz, name) == 0);
  return std::string(&value[0], sz - 1);
#else
  const char* const value = std::getenv(name);
  return value ? value : "";
#endif
}

bool EnvironmentVariablesManager::IsOSVariableSet(const char* name) {
#ifdef WINDOWS
  size_t sz = 0;
  assert(getenv_s(&sz, NULL, 0, name) == 0);
  return sz > 0;
#else
  const char* value = std::getenv(name);
  return value != NULL && *value != '\0';
#endif
}

EnvironmentVariablesManager::VariableContainer 
EnvironmentVariablesManager::PairToContainer(const std::string& name,
                                             const std::string& value) const {
  VariableContainer pair;                                            
  std::copy(name.begin(), name.end(), std::back_inserter(pair));
  pair.push_back('=');
  std::copy(value.begin(), value.end(), std::back_inserter(pair));
  pair.push_back('\0');
  return pair;
}
