const fs = require('fs');
const os = require('os');
const input = fs.readFileSync('./input.txt', {encoding: 'utf8', flag: 'r'});
const BALLS_LIMITS = {red: 12, green: 13, blue: 14};
const regex = new RegExp("(?<balls>\\d+) (?<color>" + Object.keys(BALLS_LIMITS).join('|') + ")", 'gm');

const result = input.split(os.EOL).reduce((accumulator, line) => {
    let [pre, post] = line.split(':');
    let id = Number(pre.substring(4));

    return (post.split(';').every((round) => {
        return [...round.matchAll(regex)].every((match) => Number.parseInt(match.groups.balls) <= BALLS_LIMITS[match.groups.color])
    })) ? accumulator + id : accumulator;

}, 0);
console.log(result);