#ifndef ENVIRONMENT_VARIABLE_MANAGER_H
#define ENVIRONMENT_VARIABLE_MANAGER_H

#include <string>
#include <vector>
#include <map>

class EnvironmentVariablesManager {
 public:
  typedef std::vector<char> VariableContainer;
  typedef std::map<std::string, VariableContainer> Variables;

  EnvironmentVariablesManager() {}

  void put(const std::string& name, const std::string& value);
  std::string get(const std::string& name) const;
  void del(const std::string& name);

  static void PutOSVariable(char* value);
  static std::string GetOSVariable(const char* name);
  static bool IsOSVariableSet(const char* name);

 private:
  VariableContainer PairToContainer(const std::string& name,
                                    const std::string& value) const;
  Variables vars_;

  // This class is not copiable.
  EnvironmentVariablesManager(const EnvironmentVariablesManager&);
  void operator=(const EnvironmentVariablesManager&);
};

#endif
