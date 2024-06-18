import { expect } from 'chai';
import { add, subtract } from '../src/calculator.js';

describe('Calculator', () => {
    it('should add two numbers correctly', () => {
        expect(add(1, 2)).to.equal(3);
    });

    it('should subtract two numbers correctly', () => {
        expect(subtract(5, 3)).to.equal(2);
    });
});
