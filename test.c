int main()
{
  int a;
  a = 15;
  int b;
  b = 3;
  return add(a, b);
}

int add(int x, int y)
{
  int r;
  int i;
  r = 0;
  for (i = 0; i < 10; i = i + 1)
  {
    int z;
    z = 1;
    r = r + x + y;
  }
  return r;
}
