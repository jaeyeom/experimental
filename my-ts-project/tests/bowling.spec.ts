import { calculateScore } from '../src/bowling.js';
import { expect } from 'chai';
import * as fc from 'fast-check';

describe('Bowling', () => {
    it('should produce zero for 20 zero pins', () => {
        expect(calculateScore(Array(20).fill(0))).to.equal(0);
    });

    it('should produce the sum of pins', () => {
        expect(calculateScore(Array(20).fill(1))).to.equal(20);
        expect(calculateScore(Array(20).fill(2))).to.equal(40);
    });

    it('should handle spare', () => {
        expect(calculateScore(Array(21).fill(5))).to.equal(150);
    });

    it('should handle strikes', () => {
        expect(calculateScore(Array(12).fill(10))).to.equal(300);
    });

    it('should handle all strikes but last open frame', () => {
        const pins = Array(9).fill(10);
        pins.push(3);
        pins.push(4);
        expect(calculateScore(pins)).to.equal(257);
    });
});

class RandomRoller {
    private remainingPins: number;
    /** A function that returns integer in [0, n] inclusively. */
    private randomInt: (n: number) => number;

    constructor(randomInt: (n: number) => number) {
        this.remainingPins = 10;
        this.randomInt = randomInt;
    }

    randomRoll(): number {
        const roll = this.randomInt(this.remainingPins);
        this.remainingPins -= roll;
        return roll;
    }

    empty(): boolean {
        return this.remainingPins === 0;
    }

    reset(): void {
        this.remainingPins = 10;
    }
}

function generateValidPinSequence(randomInt: (n: number) => number): number[] {
    const pins = [];
    // Slightly more likely to clear.
    const roller = new RandomRoller(randomInt);
    for (let frame = 0; frame < 9; frame++) {
        roller.reset();
        pins.push(roller.randomRoll());
        if (!roller.empty()) {
            pins.push(roller.randomRoll());
        }
    }
    // Last Frame
    roller.reset();
    pins.push(roller.randomRoll());
    if (roller.empty()) {
        // Strike
        roller.reset();
        pins.push(roller.randomRoll());
        if (roller.empty()) {
            roller.reset();
        }
        pins.push(roller.randomRoll());
    } else {
        pins.push(roller.randomRoll());
        if (roller.empty()) {
            // Spare
            roller.reset();
            pins.push(roller.randomRoll());
        }
    }

    return pins;
}

describe('Bowling Property-based Tests', () => {
    it('should the score be in the reasonable range', () => {
        const randomIntGenerators = [
            (n: number) => Math.min(n, Math.floor(Math.random() * (n + 2))), // Average Player
            (n: number) => Math.min(n, Math.floor(Math.random() * (n + 5))), // Good Player
            (n: number) => Math.min(n, Math.floor(Math.random() * (n + 10))), // Great Player
            (n: number) => Math.min(n, Math.floor(Math.random() * Math.min(n-2, 2)))  // Desperate Player
        ]
        const validGameGenerator = fc.constantFrom(...randomIntGenerators);
        fc.assert(
            fc.property(validGameGenerator, (randomInt) => {
                const pins = generateValidPinSequence(randomInt);
                const score = calculateScore(pins);
                const sum = pins.reduce((acc, curr) => acc + curr, 0);
                expect(score).to.be.within(sum, sum*3-pins[pins.length-1]-pins[pins.length-2]);
            }),
            { numRuns: 10000 }
        );
    });
});
