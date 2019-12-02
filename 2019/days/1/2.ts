import fs from "fs-extra";

const fuel = (a: number) => {
  const v = Math.floor(a / 3) - 2;

  if (v < 0) {
    return 0;
  }

  return fuel(v) + v;
};

fs.readFile("./input.txt")
  .then(r =>
    r
      .toString()
      .split("\n")
      .map(Number)
      .map(fuel)
      .reduce((a, b) => a + b, 0)
  )
  .then(console.log);
