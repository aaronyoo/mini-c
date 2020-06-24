int main()
{
  return factorial(5);
}

int factorial(int n)
{
  int ret;
  if (n == 0)
  {
    ret = 1;
  }
  else
  {
    ret = n * factorial(n - 1);
  }
  return ret;
}
