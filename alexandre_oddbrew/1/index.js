const fs = require('fs');
const os = require('os');
const input = fs.readFileSync('./input.txt', {encoding: 'utf8', flag: 'r'});
const stringDigits = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];

let total = 0;
input.split(os.EOL).forEach((line) => {
    let matches = [...line.matchAll(/(?<=(zero)|(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)|(\d))/g)];
    if (!matches)
        return;

    let firstLast = [matches[0].find((value) => Boolean(value)), matches[matches.length - 1].find((value) => Boolean(value))]
    firstLast = firstLast.map((element) => {
        if (element.length > 1)
            return stringDigits.indexOf(element);
        return element;
    });

    total += Number.parseInt("" + firstLast[0] + firstLast[1]);
})
console.log(total)