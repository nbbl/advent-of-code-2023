const fs = require('fs');
const os = require('os');
let input = fs.readFileSync('./input.txt', {encoding: 'utf8', flag: 'r'});

const cards = input.split(os.EOL).map((line, index) => {
    let [winNumbers, ownNumbers] = line.split(':')[1].split('|').map((item) => item.trim().split(/\s+/g));
    let ownWinNumbers = winNumbers.filter(number => ownNumbers.includes(number));
    return [index, ownWinNumbers.length]
});

let index = 0, resultCards = cards.map(item => item);
while (index < resultCards.length) {
    let [cardIndex, winAmount] = [...resultCards[index]];
    if (winAmount) {
        let slice = cards.slice(cardIndex + 1, cardIndex + 1 + winAmount);
        resultCards.splice(index + 1, 0, ...slice);
    }
    index++;
}
console.log(resultCards.length)