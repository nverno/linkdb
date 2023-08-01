export const DEBUG_PREFIX = process.env.DEBUG_PREFIX ?? 'linkdb';

export const DEBUG =
  process.env.DEBUG ?? process.env.NODE_ENV === 'development'
    ? `${DEBUG_PREFIX}:*`
    : undefined;
