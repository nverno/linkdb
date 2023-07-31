DROP TABLE IF EXISTS taggings;                  -- drop before referenced tables
DROP TABLE IF EXISTS links;
DROP TABLE IF EXISTS tag_idx;
DROP TABLE IF EXISTS tags;

CREATE TABLE links (
  link_id INTEGER PRIMARY KEY,
  link_name TEXT NOT NULL,
  link_ref TEXT NOT NULL,
  link_type TEXT CHECK(link_type IN ('href', 'file')) NOT NULL DEFAULT 'href',
  notes TEXT 
);

CREATE TABLE tags (
  tag_id INTEGER PRIMARY KEY,
  tag_name TEXT NOT NULL UNIQUE
);

-- maps tags <-> links
CREATE TABLE taggings (
  link_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  FOREIGN KEY (link_id) REFERENCES links (link_id) ON DELETE CASCADE ON UPDATE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags (tag_id) ON DELETE CASCADE ON UPDATE CASCADE
);



INSERT INTO links(link_name,link_ref,link_type,notes) 
VALUES ('a', 'https://a', 'href', 'aaaaa'), 
       ('b', 'https://b', 'href', NULL),
       ('f', '~/f.txt', 'file', null);

INSERT INTO tags(tag_name) VALUES ('x'), ('y');

INSERT INTO taggings(link_id, tag_id) 
VALUES (1, 1), (1, 2), (2, 2), (3, 1);

SELECT tag_name AS 'Tags for "a"'
  FROM links
       JOIN taggings USING(link_id) 
       JOIN tags USING(tag_id)
 WHERE link_name = 'a';
