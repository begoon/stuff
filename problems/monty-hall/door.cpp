#include <iostream>
#include <set>
#include <cstdlib>
#include <cassert>
#include <ctime>

int all_doors[] = { 1, 2, 3 };

bool no_change_strategy() {
  // doors - это множество доступных дверей (1, 2, 3) для выбора игроком.
  std::set<int> doors(all_doors, all_doors + 3);

  // Выбираем истинную дверь (от 1 до 3).
  int real_door = (std::rand() % 3) + 1;

  // Выбираем первый и окончательный выбор игрока (от 1 до 3).
  int primary_choice_door = (std::rand() % 3) + 1;

  return real_door == primary_choice_door;
}

bool change_strategy() {
  // doors - это множество доступных дверей (1, 2, 3) для выбора двери,
  // открываемой ведущим после первого выбора игрока.
  std::set<int> doors(all_doors, all_doors + 3);

  // Выбираем истинную дверь (от 1 до 3).
  int real_door = (std::rand() % 3) + 1;

  // Выбираем первый выбор игрока (от 1 до 3)
  int primary_choice_door = (std::rand() % 3) + 1;

  // Исключаем из множества дверей истинную дверь и выбор игрока.
  doors.erase(real_door);
  doors.erase(primary_choice_door);
  // На всякий пожарный проверим целостность данных.
  assert(doors.size() == 1 || doors.size() == 2);

  // Из оставшихся элементов (их может быть 1 или 2 штуки) выбираем дверь,
  // которую откроет ведущий. reveal_door равно либо 1, либо 2.
  int reveal_door = (std::rand() % doors.size()) + 1;

  // i указывает на первый элемент в множестве (всего в нем 1 или 2 элемента).
  std::set<int>::const_iterator i = doors.begin();
  // Сдвигаем итератор на элемент, номер которого равен reveal_door.
  // Можно было бы написать "if (reveal_door == 2) ++i;", но цикл как-то
  // универсальнее.
  while (--reveal_door) ++i;
  reveal_door = *i;

  // 'doors2' - это множество доступных дверей (1, 2, 3) для игрока,
  // меняющего свой первоначальный выбор.
  std::set<int> doors2(all_doors, all_doors + 3);

  // Исключаем из множества дверей первый выбор игрока и
  // и дверь, открытую ведущим.
  doors2.erase(primary_choice_door);
  doors2.erase(reveal_door);
  // На всякий пожарный проверим целостность данных.
  assert(doors2.size() == 1);

  // В множестве оставшихся дверь будет только одна дверь, так как истинная 
  // дверь точно не равна двери, открытой ведущим, во второй выбор игрока
  // точно отличается от первоначального. Поэтому просто берем из этого 
  // множества первый элемент.
  int second_choice = *doors2.begin();

  return real_door == second_choice;
}

int main() {
  std::srand(std::time(0));
  int guess_on_change = 0;
  int guess_on_not_change = 0;
  int N = 100000;
  for (int i = 0; i < N; ++i) {
    if (change_strategy())
      guess_on_change = guess_on_change + 1;
    if (no_change_strategy())
      guess_on_not_change = guess_on_not_change + 1;
  }
  std::cout << "Вероятность выйграть при смене изначального выбора: "
    << guess_on_change * 1.0 / N << std::endl;
  std::cout << "Вероятность выйграть не меняя изначального выбора: "
    << guess_on_not_change * 1.0 / N << std::endl;
  return 0;
}
