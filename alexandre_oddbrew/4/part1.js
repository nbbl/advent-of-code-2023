const fs = require('fs');
const os = require('os');
let input = fs.readFileSync('./input.txt', {encoding: 'utf8', flag: 'r'});

let result = input.split(os.EOL).reduce((acc, line) => {
    let [winNumbers, ownNumbers] = line.split(':')[1].split('|').map((item) => item.trim().split(/\s+/g));
    let ownWinNumbers = winNumbers.filter(number => ownNumbers.includes(number));
    if (ownWinNumbers.length)
        return acc + 2 ** (ownWinNumbers.length - 1)
    return acc
}, 0)
console.log(result);