using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace Snake
{
    class Snake
    {
        public int Width { get; set; }
        public int Height { get; set; }
        public int InitialSnakeLength { get; set; }

        private class Coordinates
        {
            public int X { get; set; }
            public int Y { get; set; }

            public override bool Equals(Object obj)
            {
                if (ReferenceEquals(null, obj)) return false;
                if (ReferenceEquals(this, obj)) return true;
                var coordinates = (Coordinates) obj;
                return coordinates.X == X && coordinates.Y == Y;
            }

            public override int GetHashCode()
            {
                unchecked
                {
                    return (X*397) ^ Y;
                }
            }

            public static Coordinates operator +(Coordinates lhs, Coordinates rhs)
            {
                return new Coordinates { X = lhs.X + rhs.X, Y = lhs.Y + rhs.Y };
            }

            public static Coordinates operator -(Coordinates lhs, Coordinates rhs)
            {
                return new Coordinates { X = lhs.X - rhs.X, Y = lhs.Y - rhs.Y };
            }

            public Coordinates GenerateRandomly(int width, int height, int padding = 0)
            {
                var randomizer = new Random((int)DateTime.Now.Ticks);
                X = randomizer.Next(0 + padding, width - padding);
                Y = randomizer.Next(0 + padding, height - padding);
                return this;
            }
        }

        private class Direction : Coordinates { }

        private static readonly Direction Up = new Direction() { X = 0, Y = -1 };
        private static readonly Direction Down = new Direction() { X = 0, Y = +1 };
        private static readonly Direction Left = new Direction() { X = -1, Y = 0 };
        private static readonly Direction Right = new Direction() { X = +1, Y = 0 };
        private static readonly Direction[] Directions = new Direction[4] { Up, Down, Left, Right };

        private readonly List<StringBuilder> _map = new List<StringBuilder>();

        private Direction _direction;
        private List<Coordinates> _snake;
        private int _snakeGrowth;

        private Coordinates SnakeHead { get { return _snake.First(); } set { _snake[0] = value; } }

        private readonly Coordinates _rabbit = new Coordinates();

        private void GenerateMap()
        {
            Enumerable.Range(0, Height).ToList().ForEach(_ => _map.Add(new StringBuilder("".PadRight(Width, ' '))));
        }

        private void PlaceSnakeRandomly()
        {
            _snake = new List<Coordinates>() { new Coordinates().GenerateRandomly(Width, Height, InitialSnakeLength) };
            _direction = Directions[new Random((int)DateTime.Now.Ticks).Next(Directions.Length)];
            Enumerable.Range(1, InitialSnakeLength - 1).ToList().ForEach((i) => _snake.Add(_snake[i - 1] - _direction));
        }

        private static void ClearScreen()
        {
            Console.Clear();
        }

        private static char ReadKey()
        {
            return Console.ReadKey().KeyChar;
        }

        private static bool IsKeyPressed()
        {
            return Console.KeyAvailable;
        }

        private void DrawMap()
        {
            ClearScreen();
            Console.WriteLine("+" + "".PadRight(Width, '-') + "+");
            _map.ForEach(line => Console.WriteLine("|" + line + "|"));
            Console.WriteLine("+" + "".PadRight(Width, '-') + "+");
        }

        private static readonly Hashtable SnakeHeadCharacters = new Hashtable() {{Up, '^'}, {Down, 'v'}, {Left, '<'}, {Right, '>'}};
        private char SnakeHeadCharacter()
        {
            return (char)SnakeHeadCharacters[_direction];
        }

        private char SnakeBodyCharacter(Coordinates position)
        {
            return position == SnakeHead ? SnakeHeadCharacter() : '@';
        }

        private void DrawSnake(bool show = true)
        {
            _snake.ForEach(position => _map[position.Y][position.X] = show ? SnakeBodyCharacter(position) : ' ');
        }

        private void ClearSnake()
        {
            DrawSnake(false);
        }

        private delegate Direction KeyPressAction(Direction direction);

        private readonly Dictionary<char, KeyPressAction> _actions = new Dictionary<char, KeyPressAction> {
                                                                {'w',  (direction) => { 
                                                                    if (direction != Down) direction = Up;
                                                                    return direction; } 
                                                                },
                                                                {'s',  (direction) => { 
                                                                    if (direction != Up) direction = Down;
                                                                    return direction; } 
                                                                },
                                                                {'a',  (direction) => { 
                                                                    if (direction != Right) direction = Left;
                                                                    return direction; } 
                                                                },
                                                                {'d',  (direction) => { 
                                                                    if (direction != Left) direction = Right;
                                                                    return direction; } 
                                                                },
                                                                {'q',  (_) => {
                                                                    Environment.Exit(0);
                                                                    return _; } 
                                                                },
                                                            };

        void ChangeDirection()
        {
            KeyPressAction action;
            if (_actions.TryGetValue(ReadKey(), out action))
                _direction = action(_direction);
        }

        private void CheckSnakeHeadCoordinates()
        {
            if (SnakeHead.X < 0) SnakeHead.X = Width - 1;
            if (SnakeHead.X >= Width) SnakeHead.X = 0;
            if (SnakeHead.Y < 0) SnakeHead.Y = Height - 1;
            if (SnakeHead.Y >= Height) SnakeHead.Y = 0;
        }

        private bool GameOver()
        {
            return _snake.IndexOf(SnakeHead, 1) != -1;
        }

        private void PlaceRabbit()
        {
            do
            {
                _rabbit.GenerateRandomly(Width, Height);
            } while (_map[_rabbit.Y][_rabbit.X] != ' ');
            _map[_rabbit.Y][_rabbit.X] = '+';
        }

        private void NextStep()
        {
            ClearSnake();

            _snake.Insert(0, SnakeHead + _direction);

            if (_snakeGrowth > 0)
                _snakeGrowth -= 1;
            else
                _snake.RemoveAt(_snake.Count - 1);

            CheckSnakeHeadCoordinates();

            if (SnakeHead.Equals(_rabbit))
            {
                _snakeGrowth += 1;
                PlaceRabbit();
            }

            DrawSnake();

            if (GameOver())
            {
                Environment.Exit(1);
            }
        }

        public void Game()
        {
            GenerateMap();
            PlaceSnakeRandomly();
            PlaceRabbit();
            DrawSnake();
            DrawMap();

            while (true)
            {
                if (IsKeyPressed()) 
                    ChangeDirection();
                NextStep();
                DrawMap();

                Thread.Sleep(200);
            }
        }
    }

    class Program
    {
        static void Main()
        {
            new Snake(){ Width = 40, Height = 20, InitialSnakeLength = 3 }.Game();
        }
    }
}
