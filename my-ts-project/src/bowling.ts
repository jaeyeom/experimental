import assert from 'assert';

/**
 * Represents a pin count in a single roll of bowling. Must be an integer value
 * between 0 and 10 inclusive.
 */
type Pin = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10;

function isValidPin(pin: number): pin is Pin {
    return Number.isInteger(pin) && 0 <= pin && pin <= 10;
}

function assertArrayOfPins(pins: number[]): asserts pins is Pin[] {
    assert(pins.every(isValidPin),
           'All pins must be integers between 0 and 10.');
}

function assertValidScore(score: number): void {
    assert(Number.isInteger(score),
           'Invalid pin sequence resulting in a non-integer score.')
    assert(0 <= score && score <= 300,
           `Invalid score 0 <= ${score} <= 300.`);
}

function assertRemainingThrows(remainingFrames: number, remainingThrows: number): void {
    assert(remainingFrames + 1 <= remainingThrows && remainingThrows <= remainingFrames * 2 + 1,
           `The remaining frames are off the range ${remainingFrames+1} <= ${remainingThrows} <= ${remainingFrames*2+1}.`);
}

export function calculateScore(pins: number[]): number {
    assertArrayOfPins(pins);
    let score = 0;
    let i = 0;
    for (let frame = 0; frame < 10; frame++) {
        assertRemainingThrows(10 - frame, pins.length - i);
        if (pins[i] === 10) {
            // Strike
            score += pins[i] + pins[i+1] + pins[i+2];
            i += 1;
        } else if (pins[i] + pins[i+1] === 10) {
            // Spare
            score += pins[i] + pins[i+1] + pins[i+2];
            i += 2;
        } else {
            // Open frame
            const currentScore = pins[i] + pins[i+1];
            assert(0 <= currentScore && currentScore < 10, "Invalid pin count in the open frame.");
            score += currentScore;
            i += 2;
        }
    }
    assert((pins[i-1] === 10 && i+2==pins.length)
        || (pins[i-2]+pins[i-1] === 10 && i+1==pins.length)
        || i == pins.length,
          "No remaining throws are expected.")
    assertValidScore(score);
    return score;
}
