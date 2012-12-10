// Originally taken from http://itblogs.org/c-konsolnaya-zmejka/.

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#ifdef WINDOWS
// Библиотека, нужна для использования функции Sleep().
#include <windows.h>
// Библиотека, нужна для использования функций kbhit() и getch().
#include <conio.h>
#else
#include <unistd.h>
#include <termios.h>
#include <sys/select.h>

#define STDIN_FILENO 0
#define NB_DISABLE 0
#define NB_ENABLE 1

#define Sleep(x) usleep(x*1000)

int kbhit() {
  struct timeval tv;
  fd_set fds;
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  FD_ZERO(&fds);
  FD_SET(STDIN_FILENO, &fds);
  select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
  return FD_ISSET(STDIN_FILENO, &fds);
}

void nonblock(int state) {
  struct termios ttystate;

  // Get the terminal state.
  tcgetattr(STDIN_FILENO, &ttystate);

  if (state == NB_ENABLE) {
    // Turn off canonical mode.
    ttystate.c_lflag &= ~ICANON;
    // Minimum of number input read.
    ttystate.c_cc[VMIN] = 1;
  } else if (state == NB_DISABLE) {
    // Turn on canonical mode.
    ttystate.c_lflag |= ICANON;
  }
  // Set the terminal attributes.
  tcsetattr(STDIN_FILENO, TCSANOW, &ttystate);
}

int getch() {
  return fgetc(stdin);
}

#endif
// Этот массив содержит в себе все возможные консольные цвета.
const char* main_color[] = {
"color 01", "color 02", "color 03", "color 04", "color 05", "color 06",
"color 07", "color 08", "color 09", "color 0A", "color 0B", "color 0C",
"color 0D", "color 0E", "color 0F", "color 10", "color 12", "color 13",
"color 14", "color 15", "color 16", "color 17", "color 18", "color 19",
"color 1A", "color 1B", "color 1C", "color 1D", "color 1E", "color 1F",
"color 20", "color 21", "color 23", "color 24", "color 25", "color 26",
"color 27", "color 28", "color 29", "color 2A", "color 2B", "color 2C",
"color 2D", "color 2E", "color 2F", "color 30", "color 31", "color 32",
"color 34", "color 35", "color 36", "color 37", "color 38", "color 39",
"color 3A", "color 3B", "color 3C", "color 3D", "color 3E", "color 3F",
"color 40", "color 41", "color 42", "color 43", "color 45", "color 46",
"color 47", "color 48", "color 49", "color 4A", "color 4B", "color 4C",
"color 4D", "color 4E", "color 4F", "color 50", "color 51", "color 52",
"color 53", "color 54", "color 56", "color 57", "color 58", "color 59",
"color 5A", "color 5B", "color 5C", "color 5D", "color 5E", "color 5F",
"color 60", "color 61", "color 62", "color 63", "color 64", "color 65",
"color 67", "color 68", "color 69", "color 6A", "color 6B", "color 6C",
"color 6D", "color 6E", "color 6F", "color 70", "color 71", "color 72",
"color 73", "color 74", "color 75", "color 76", "color 78", "color 79",
"color 7A", "color 7B", "color 7C", "color 7D", "color 7E", "color 7F",
"color 80", "color 81", "color 82", "color 83", "color 84", "color 85",
"color 86", "color 87", "color 89", "color 8A", "color 8B", "color 8C",
"color 8D", "color 8E", "color 8F", "color 90", "color 91", "color 92",
"color 93", "color 94", "color 95", "color 96", "color 97", "color 98",
"color 9A", "color 9B", "color 9C", "color 9D", "color 9E", "color 9F",
"color A0", "color A1", "color A2", "color A3", "color A4", "color A5",
"color A6", "color A7", "color A8", "color A9", "color AB", "color AC",
"color AD", "color AE", "color AF", "color B0", "color B1", "color B2",
"color B3", "color B4", "color B5", "color B6", "color B7", "color B8",
"color B9", "color BA", "color BC", "color BD", "color BE", "color BF",
"color C0", "color C1", "color C2", "color C3", "color C4", "color C5",
"color C6", "color C7", "color C8", "color C9", "color CA", "color CB",
"color CD", "color CE", "color CF", "color D0", "color D1", "color D2",
"color D3", "color D4", "color D5", "color D6", "color D7", "color D8",
"color D9", "color DA", "color DB", "color DC", "color DE", "color DF",
"color E0", "color E1", "color E2", "color E3", "color E4", "color E5",
"color E6", "color E7", "color E8", "color E9", "color EA", "color EB",
"color EC", "color ED", "color EF", "color F0", "color F1", "color F2",
"color F3", "color F4", "color F5", "color F6", "color F7", "color F8",
"color F9", "color FA", "color FB", "color FC", "color FD", "color FE"};

// snake_size - размер змейки.
// change_x, change_y - в какую сторону движется змейка.
// coordinates_x[1000], coordinates_y[1000] - массивы, хранящие координаты
//                                            частей тела змейки.
// Kоординаты головы змейки хранятся в coordinates_x[1], coordinates_y[1].
// food_x, food_y - координаты еды.

int snake_size, change_x, change_y, coordinates_x[1000], coordinates_y[1000];
int food_x = -1, food_y = -1;

// symbol - хранит в себе ASCII код нажатой клавиши.
// a[1000][1000] - наша таблица, в которой происходит вся игра.
char symbol, a[1000][1000];

// Константы:
// N - размер таблицы, а именно высота.
// M - ширина таблицы.
// INTERVAL - интервал в миллисекундах, через каждый этот промежуток
//            времени змейка будет передвигаться.
const int N = 13, M = 17, INTERVAL = 200;

// функция изменения цвета консоли.
void change_color() {
  // Выбираем рандомный цвет.
  const int x = std::rand() % 240;
  // Меняем цвет.
  system(main_color[x]);
}

// функция, считывающая нажатую клавишу.
void change_direction() {
  // Cчитываем нажатую клавишу с помощью функции getch().
  symbol = getch();
  switch (symbol) {
    // Управление змейкой у нас через wasd.
    case 'w': if (change_x != 1 || change_y != 0) {
                change_x = -1; change_y = 0;
              }
              break;
    case 'a': if (change_x != 0 || change_y != 1) {
                change_x = 0; change_y = -1;
              }
              break;
    case 's': if (change_x != -1 || change_y != 0) {
                change_x = 1; change_y = 0;
              }
              break;
    case 'd': if (change_x != 0 || change_y != -1) {
                change_x = 0; change_y = 1;
              }
              break;
#ifndef WINDOWS
    case 'q': nonblock(NB_DISABLE); std::exit(0);
#endif
#ifdef WINDOWS
    // Если нажат пробел, то меняем цвет консоли.
    case 32: change_color(); break;
#endif
    default: break;
  }
}

// функция для вывода таблицы
void show_table() {
  // Очищаем консоль.
#ifdef WINDOWS
  system("cls");
#else
  system("clear");
#endif

  // Выводим таблицу и края.
  for (int i = 0; i <= N + 1; ++i)
    for (int j = 0; j <= M + 1; ++j)
      std::cout
        << (i == 0 || j == 0 || i == N + 1 || j == M + 1 ?
           '#' : a[i][j])
        << (j <= M ? "" : "\n");
}

// Очищаем координаты, в которых располагалась змейка.
void clear_snake_on_table() {
  for (int i = 1; i <= snake_size; ++i)
    a[coordinates_x[i]][coordinates_y[i]] = ' ';
}

// Красим координаты змейки.
void show_snake_on_table() {
  // Изменяем тип головы.
  if (change_x == 1 && change_y == 0)
    a[coordinates_x[1]][coordinates_y[1]] = 'v';
  if (change_x == -1 && change_y == 0)
    a[coordinates_x[1]][coordinates_y[1]] = '^';
  if (change_x == 0 && change_y == 1)
    a[coordinates_x[1]][coordinates_y[1]] = '>';
  if (change_x == 0 && change_y == -1)
    a[coordinates_x[1]][coordinates_y[1]] = '<';

  // Красим змейку.
  for (int i = 2; i <= snake_size; ++i)
    a[coordinates_x[i]][coordinates_y[i]] = '@';
}

// Проверяем, съела ли змейка саму себя.
bool game_over() {
  for (int i = 2; i <= snake_size; ++i)
    // Eсли координаты головы змейки равны координате какой-либо части тела
    // змейки, то змейка съела саму себя.
    if (coordinates_x[1] == coordinates_x[i] &&
        coordinates_y[1] == coordinates_y[i])
      return true;
  // Если все координаты различны, то все в порядке - играем дальше.
  return false;
}

// Проверяем, не вышла ли змейка за поле, если да то возвращаем ее обратно.
void check_coordinates() {
  if (coordinates_x[1] > N) coordinates_x[1] = 1;
  if (coordinates_x[1] < 1) coordinates_x[1] = N;
  if (coordinates_y[1] > M) coordinates_y[1] = 1;
  if (coordinates_y[1] < 1) coordinates_y[1] = M;
}

// функция следующего хода, в которой наша змейка сдвигается в сторону
// на 1 ячейку.
void next_step() {
  // Чистим таблицу от змейки.
  clear_snake_on_table();

  // Передвигаем тело змейки.
  for (int i = snake_size; i >= 2; --i) {
    coordinates_x[i] = coordinates_x[i - 1];
    coordinates_y[i] = coordinates_y[i - 1];
  }

  // Передвигаем голову змейки.
  coordinates_x[1] += change_x;
  coordinates_y[1] += change_y;

  // Проверяем в порядке ли координаты.
  check_coordinates();

  // Если голова змейки там же где и еда, то увеличиваем размер змейки
  // и очищаем координаты змейки.
  if (coordinates_x[1] == food_x && coordinates_y[1] == food_y) {
    snake_size++;
    food_x = -1;
    food_y = -1;
  }

  // Рисуем змейку.
  show_snake_on_table();

  // Если змея укусила себя.
  if (game_over()) {
    // Cообщаем всю правду о игроке.
    std::cout << "You're looser! \n";
    // Приостанавливаем игру.
#ifdef WINDOWS
    system("pause");
#endif
    // Завершаем программу.
    std::exit(0);
  }
}

// функция проверки на наличие еды на карте.
bool food_check() {
  // Если координаты еды неопределенны, то возвращаем true.
  if (food_x == -1 && food_y == -1) return false;
  // В остальных случаях false.
  return true;
}

// функция добавления еды на карту.
void place_food() {
  std::srand(std::time(NULL));
  // Cтавим в рандомное место еду.
  for (int i = 1; i <= 9; ++i) {
    int x = std::rand(), y = std::rand();
    if (x < 0) x *= -1;
    if (y < 0) y *= -1;
    x %= (N + 1);
    y %= (M + 1);
    if (x == 0) ++x;
    if (y == 0) ++y;
    if (a[x][y] != '@' && a[x][y] != 'v' && a[x][y] != '^' &&
        a[x][y] != '<' && a[x][y] != '>') {
      food_x = x;
      food_y = y;
      a[x][y] = '+';
      return;
    }
  }
}

// Начальные установки.
void standart_settings() {
  // Размер змеи - 2.
  snake_size = 2;

  // Змейка занимает две клетки вправо от координаты {1,1}.
  coordinates_x[1] = 1;
  coordinates_y[1] = 2;
  coordinates_x[2] = 1;
  coordinates_y[2] = 1;

  // Змейка движется вправо.
  change_x = 0;
  change_y = 1;
}

int main() {
  // Задаем стандартные настройки.
  standart_settings();

#ifndef WINDOWS
  std::memset(a, ' ', sizeof(a));
  nonblock(NB_ENABLE);
#endif

  // Бесконечный цикл.
  while (true) {
    // Если нажата клавиша, обрабатываем нажатую клавишу.
    if (kbhit() != 0)
     change_direction();

    // Двигаем змейку.
    next_step();

    // Если нет еды, то ставим ее.
    if (!food_check())
      place_food();

    // Рисуем змейку.
    show_table();

    // "Усыпляем" программу на заданный интервал.
    Sleep(INTERVAL);
  }
}
