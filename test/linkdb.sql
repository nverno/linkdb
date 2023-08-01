DROP TABLE IF EXISTS link_tags;                 -- drop before referenced tables
DROP TABLE IF EXISTS group_tags;
DROP TABLE IF EXISTS links;
DROP TABLE IF EXISTS tag_idx;
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS groups;

CREATE TABLE links (
  link_id INTEGER PRIMARY KEY,
  title TEXT NOT NULL,
  url TEXT NOT NULL,
  type TEXT CHECK(type IN ('href', 'file')) NOT NULL DEFAULT 'href',
  hits INTEGER NOT NULL DEFAULT(0),
  accessed_at DATETIME NOT NULL DEFAULT current_timestamp,
  description TEXT 
);

CREATE TABLE tags (
  tag_id INTEGER PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  alias TEXT
);

-- maps tags <-> links
CREATE TABLE link_tags (
  link_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  FOREIGN KEY (link_id) REFERENCES links (link_id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags (tag_id) ON DELETE CASCADE,
  PRIMARY KEY (link_id, tag_id)
);

CREATE TABLE groups (
  group_id INTEGER PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT
);

-- tags <-> groups
CREATE TABLE group_tags (
  group_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  FOREIGN KEY (group_id) REFERENCES groups (group_id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags (tag_id) ON DELETE CASCADE,
  PRIMARY KEY (tag_id, group_id)
);

---------------------------------------------------------------------
--- Insert

INSERT INTO links(title, url, type, description) 
VALUES ('a', 'https://a', 'href', 'aaaaa'), 
       ('b', 'https://b', 'href', NULL),
       ('f', '~/f.txt', 'file', null);

INSERT INTO tags(name, alias) 
VALUES ('x-mode', 'x'), ('y-mode', 'y');

INSERT INTO link_tags(link_id, tag_id) 
VALUES (1, 1), (1, 2), (2, 2), (3, 1);

INSERT INTO groups(name, description)
VALUES ('xy', 'both x and y');

INSERT INTO group_tags(group_id, tag_id)
SELECT group_id, tag_id
  FROM tags t, (SELECT group_id FROM groups g WHERE g.name = 'xy')
 WHERE t.name IN ('x', 'y') OR t.alias IN ('x', 'y');

--- Update

UPDATE tags 
   SET alias = 'xx', name = 'xx-mode'
 WHERE name = 'x-mode';

UPDATE links
   SET hits = hits + 1, accessed_at = CURRENT_TIMESTAMP, type = 'href'
 WHERE title = 'a';

---------------------------------------------------------------------
--- Queries 

-- Tags for link
SELECT t.tag_id, IFNULL(t.alias, t.name) AS 'tag'
  FROM links l
       JOIN link_tags USING(link_id) 
       JOIN tags t USING(tag_id)
 WHERE l.title = 'a';

SELECT t.tag_id, IFNULL(t.alias, t.name) AS 'tag'
  FROM tags t
 WHERE tag_id IN (
   SELECT tag_id FROM links l JOIN link_tags USING (link_id)
    WHERE l.title = 'a'
 );

-- links for tag
SELECT l.* 
  FROM links l JOIN link_tags USING (link_id)
       JOIN tags t USING (tag_id)
 WHERE t.name = 'x' OR t.alias = 'x';

SELECT l.*
  FROM links l
 WHERE link_id IN (
   SELECT link_id FROM tags t JOIN link_tags USING (tag_id)
    WHERE t.name = 'x' OR t.alias = 'x'
 );

-- tags for group
SELECT t.*
  FROM groups g
       JOIN group_tags USING (group_id)
       JOIN tags t USING (tag_id)
 WHERE g.name = 'xy';

SELECT t.*
  FROM tags t 
 WHERE tag_id IN (
   SELECT tag_id 
     FROM groups g JOIN group_tags USING (group_id)
    WHERE g.name = 'xy'
 );

-- links for group
SELECT DISTINCT l.*
  FROM links l
       JOIN link_tags USING (link_id)
       JOIN group_tags USING (tag_id)
       JOIN groups g USING (group_id)
 WHERE g.name = 'xy';

SELECT DISTINCT l.*
  FROM links l JOIN link_tags USING (link_id)
 WHERE tag_id IN (
   SELECT tag_id
     FROM groups g JOIN group_tags USING (group_id)
    WHERE g.name = 'xy'
 );

-- groups for link
SELECT DISTINCT g.*
  FROM groups g JOIN group_tags USING (group_id)
       JOIN link_tags USING (tag_id)
       JOIN links l USING (link_id)
 WHERE l.title = 'a';

SELECT DISTINCT g.*
  FROM groups g JOIN group_tags USING (group_id)
 WHERE tag_id IN (
   SELECT tag_id
     FROM links l JOIN link_tags USING (link_id)
    WHERE l.title = 'a'
);
