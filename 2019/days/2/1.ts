import fs from "fs-extra";

fs.readFile("./input.txt")
  .then(r =>
    r
      .toString()
      .split("\n")
      .map(Number)
      .map(v => Math.floor(v / 3) - 2)
      .reduce((a, b) => a + b, 0)
  )
  .then(console.log);
