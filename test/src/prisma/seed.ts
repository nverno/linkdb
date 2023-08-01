import seedrandom from 'seedrandom';
import prisma from '../client';
import { faker } from '@faker-js/faker';
import Debug from 'debug';

// const { DEBUG = 'seed:*' } = process.env;
// eslint-disable-next-line
const debug = Debug('seed');
Debug.enable(process.env.DEBUG || 'seed');

const SEED = 1647142082296;
const useSeededRNG = true;
const seedDate = new Date(SEED);
const randomTimestampSeed = seedDate.toISOString();

let rng = seedrandom();
if (useSeededRNG) {
  rng = seedrandom(randomTimestampSeed);
  // setRandom(rng);
  faker.seed(SEED);
}

const randomInt = (min: number, max: number) => {
  min = Math.ceil(min);
  return Math.floor(rng() * (Math.floor(max) - min + 1)) + min;
};

const maptimes = (n: number, fn) =>
  Array.from({ length: n }, (x, i) => i).map((i) => fn(i));
