const fs = require('fs');
const os = require('os');
let input = fs.readFileSync('./input.txt', {encoding: 'utf8', flag: 'r'});
const LINE_SIZE = input.indexOf(os.EOL) + 1;
input = input.replace(/\s+/g, '.');

let numbers = [], symbols = [];
[...input.matchAll(/(\d+)|([^.\d])/g)].forEach((match) => {
    isNaN(match[0]) ? symbols.push({value: match[0], index: match.index}) : numbers.push({
        value: Number.parseInt(match[0]),
        startIndex: match.index,
        endIndex: match.index + match[0].length - 1
    })
})

let flaggedNumbers = new Set();
symbols.forEach((symbol) => {
    numbers.filter((number) => {
        return number.startIndex === symbol.index + 1 || number.endIndex === symbol.index - 1
            || symbol.index - LINE_SIZE >= number.startIndex - 1 && symbol.index - LINE_SIZE <= number.endIndex + 1
            || symbol.index + LINE_SIZE >= number.startIndex - 1 && symbol.index + LINE_SIZE <= number.endIndex +1
    }).forEach((item) => flaggedNumbers.add(item));
})
console.log(Array.from(flaggedNumbers).reduce((accumulator, item) => accumulator + item.value, 0));