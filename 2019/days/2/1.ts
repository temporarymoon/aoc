import fs from "fs";

const content = fs.readFileSync("./input.txt");
const array = content
  .toString()
  .split(",")
  .map(Number);

array[1] = 12;
array[2] = 2;

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

      console.log("doing a product");

      continue;
    case 99:
      // force break out of loop
      index = array.length;
      console.log("out", index);

      continue;
    default:
      console.log("wrong");
  }
}

console.log(array[0]);
