function loop_types(obj, arr) {
  let i = 0;
  while (i < 3) {
    i++;
    console.log(i);
  }

  do {
    i++;
    console.log(i);
  } while (i < 5);

  for (let j = 0; j < 3; j++) {
    i += j;
    console.log(i);
  }

  for (const key in obj) {
    console.log(key);
  }

  for (const val of arr) {
    console.log(val);
  }

  return i;
}
