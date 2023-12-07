const fs = require('fs');
const os = require('os');
const input = fs.readFileSync('./input.txt', {encoding: 'utf8', flag: 'r'});
const BALLS_LIMITS = {red: 12, green: 13, blue: 14};
const regex = new RegExp("(?<balls>\\d+) (?<color>" + Object.keys(BALLS_LIMITS).join('|') + ")", 'gm');

const result = input.split(os.EOL).reduce((accumulator, line) => {
    let minimums = {};
    line.split(':')[1].split(';').forEach((round) => {
        [...round.matchAll(regex)].forEach((match) => {
            let colorAmount = Number.parseInt(match.groups.balls);
            minimums[match.groups.color] = (minimums[match.groups.color] || 0) > colorAmount ? minimums[match.groups.color] : colorAmount;
        })
    })

    return accumulator + Object.values(minimums).reduce((stack, currentValue) => stack * currentValue, 1);
}, 0);
console.log(result);