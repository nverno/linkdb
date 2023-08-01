;;; linkdb.el --- search/store links by tags in sqlite  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.
;;
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/help-utils
;; Package-Requires:
;; Created: 31 July 2023
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Store tagged links in sqlite database for easy access.
;;
;;; Code:
(require 'sqlite)

(declare-function sqlite-execute "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlite-open "sqlite.c")
(declare-function sqlite-pragma "sqlite.c")
(declare-function sqlite-transaction "sqlite.c")
(declare-function sqlite-commit "sqlite.c")

(defcustom linkdb-directory (expand-file-name "linkdb/" user-emacs-directory)
  "Directory to store links database."
  :type 'file
  :group 'files)

(defun linkdb--clean (str)
  (if (stringp str )
      (replace-regexp-in-string " ?\\([(),]\\) ?" "\\1" (string-clean-whitespace str))
    str))

(defconst linkdb--schema
  (eval-when-compile
    (mapcar #'linkdb--clean '("
CREATE TABLE links (
  link_id INTEGER PRIMARY KEY,
  title TEXT NOT NULL,
  url TEXT NOT NULL,
  type TEXT CHECK(type IN ('href', 'file')) NOT NULL DEFAULT 'href',
  hits INTEGER NOT NULL DEFAULT(0),
  accessed_at DATETIME NOT NULL DEFAULT current_timestamp,
  description TEXT
)""
CREATE TABLE tags (tag_id INTEGER PRIMARY KEY,name TEXT NOT NULL UNIQUE,alias TEXT)" "
CREATE TABLE link_tags (
  link_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  FOREIGN KEY (link_id) REFERENCES links (link_id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags (tag_id) ON DELETE CASCADE,
  PRIMARY KEY (link_id, tag_id)
)""
CREATE TABLE groups (
  group_id INTEGER PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT
)""
CREATE TABLE group_tags (
  group_id INTEGER NOT NULL,
  tag_id INTEGER NOT NULL,
  FOREIGN KEY (group_id) REFERENCES groups (group_id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags (tag_id) ON DELETE CASCADE,
  PRIMARY KEY (tag_id, group_id)
)")))
  "Sqlite tables.")

(defvar linkdb--db nil)

;; Load / create database
;; modified from `multisession--ensure-db' in multisession.el
(defun linkdb--ensure-db ()
  (unless linkdb--db
    (let* ((file (expand-file-name "linkdb.sqlite" linkdb-directory))
           (dir (file-name-directory file)))
      (unless (file-exists-p dir)
        (make-directory dir))
      (setq linkdb--db (sqlite-open file)))
    (with-sqlite-transaction linkdb--db
      ;; see notes about config options in comments in multisession.el
      (sqlite-pragma linkdb--db "journal_mode = WAL")
      (sqlite-pragma linkdb--db "synchronous = NORMAL")
      (unless (sqlite-select
               linkdb--db
               "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'links'")
        (sqlite-pragma linkdb--db "auto_vacuum = FULL")
        ;; create tables
        (dolist (tab linkdb--schema)
          (sqlite-execute linkdb--db tab))))))

(defsubst linkdb--quote-list (lst &optional sep)
  (mapconcat (lambda (e) (format "'%s'" e)) lst (or sep ", ")))

(defsubst linkdb--format-values (vals)
  (mapconcat
   (lambda (entry) (concat "(" (mapconcat #'prin1-to-string entry ", ") ")"))
   vals ", "))

(eval-when-compile
  (defmacro linkdb:sql (&rest body)
    "Remove boilerplate from sql queries, insert `linkdb--db' into queries."
    `(progn (linkdb--ensure-db)
            (with-sqlite-transaction linkdb--db
              (cl-macrolet ((sql:execute (query &optional values)
                              `(sqlite-execute linkdb--db ,(linkdb--clean query) ,values))
                            (sql:select (query &optional values)
                              `(sqlite-select linkdb--db ,(linkdb--clean query) ,values)))
                ,@body)))))

;; -------------------------------------------------------------------
;;; Insert

(defun linkdb-insert-tag (name &optional alias)
  (linkdb:sql (sql:execute "INSERT INTO tags(name, alias) VALUES(?)" (list name alias))))

(defun linkdb-insert--group (name &optional description)
  (linkdb:sql
   (sql:execute "INSERT INTO groups(name, description VALUES(?)" (list name description))))

(defun linkdb-insert--link (link-name link-ref &optional link-type description)
  (linkdb:sql
   (sql:execute
    "INSERT INTO links(title, url, type, description) VALUES(?, ?, ?, ?)"
    (list link-name link-ref (or link-type "href") description))))

(defun linkdb-insert-link (link-name link-ref &optional link-type description &rest tags)
  (linkdb-insert--link link-name link-ref link-type description)
  (when tags
    (linkdb:sql
     (sql:execute "insert into tags(name)
select "))))

;; Insert entries into join table for LINK-ID (default last inserted row id)
;; to each tag in TAGS
(defun linkdb-insert-link-tags (&optional link-id &rest tags)
  (linkdb:sql
    (let ((vals
           (sql:select
            (format "SELECT %s as link_id, tag_id FROM tags t WHERE t.name IN (%s)"
                    (or link-id "LAST_INSERT_ROWID()")
                    (linkdb--quote-list tags)))))
      (sql:execute (format "INSERT INTO link_tags(link_id, tag_id) VALUES %s"
                           (linkdb--format-values vals))))))

(defun linkdb-insert-group-tags (group-name &rest tags)
  (let ((tags (linkdb--quote-list tags)))
    (linkdb:sql
     (sql:execute "insert or ignore into groups(name) values (?)" (list group-name))
     (sql:execute
      ;; format
      "insert into group_tags(group_id, tag_id)
select group_id, tag_id
from tags t, (select group_id from groups g where g.name = ?)
where t.name in (?) or t.alias in (?)" (list group-name tags tags)
      ;; (list group-name)
      ))))

;; -------------------------------------------------------------------
;;; Update

;; -------------------------------------------------------------------
;;; Queries

(defun linkdb-link-tags (link-title)
  "Get tags associated with link LINK-TITLE."
  (linkdb:sql (sql:select "select t.* from tags t where tag_id in (
select tag_id from links l join link_tags using (link_id)
where l.title = ?)" (list link-title))))

(defun linkdb-link-groups (link-title)
  "Get groups associated with link LINK-TITLE."
  (linkdb:sql (sql:select "select distinct g.*
from groups g join group_tags using(group_id)
where tag_id in (select tag_id from links l join link_tags using (link_id)
where l.title = ?)" (list link-title))))

(defun linkdb-tag-links (tag-name)
  "Get links associated with tag TAG-NAME."
  (linkdb:sql (sql:select "select l.* from links l where link_id in (
select link_id from tags t join link_tags using (tag_id)
where t.name = ? or t.alias = ?)" (list tag-name tag-name))))

(defun linkdb-group-tags (group-name)
  "Get tags associated with group GROUP-NAME."
  (linkdb:sql (sql:select "select t.* from tags t where tag_id in (
select tag_id from groups g join group_tags using (group_id)
where g.name = ?)" (list group-name))))

(defun linkdb-group-links (group-name)
  "Get links associated with group GROUP-NAME."
  (linkdb:sql (sql:select "select l.* from links l join link_tags using(link_id)
where tag_id in (select tag_id from groups g join group_tags using (group_id)
where g.name = ?)" (list group-name))))


;; Return list of links tagged with any tag in TAGS
(defun linkdb-links-by-tags (tags)
  ;; TODO: generate completions
  ;; (interactive (list (read-string "Tags: ")))
  (linkdb:sql
   (let ((tags (linkdb--quote-list tags)))
     (sql:select
      (format
       "SELECT DISTINCT l.*
     FROM links l JOIN link_tags USING(link_id) JOIN tags t USING(tag_id)
     WHERE t.name IN (%s) OR t.alias IN (%s)" tags tags)))))


(provide 'linkdb)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; linkdb.el ends here
