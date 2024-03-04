import fs from "fs";

const content = fs.readFileSync("./input.txt");
const originalArray = content
  .toString()
  .split(",")
  .map(Number);

for (let x = 0; x < 99; x++) {
  for (let y = 0; y < 99; y++) {
    const array = [...originalArray];

    array[1] = x;
    array[2] = y;

    for (let index = 0; index < array.length; index += 4) {
      const element = array[index];

      switch (element) {
        case 1:
          const sum = array[array[index + 1]] + array[array[index + 2]];
          array[array[index + 3]] = sum;

          continue;
        case 2:
          const product = array[array[index + 1]] * array[array[index + 2]];
          array[array[index + 3]] = product;

          continue;
        case 99:
          // force break out of loop
          index = array.length;

          continue;
        default:
          console.log("wrong");
      }
    }

    if (array[0] === 19690720) {
      console.log(x, y);

      console.log(100 * x + y);

      process.exit(0);
    }
  }
}
