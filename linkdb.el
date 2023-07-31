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

(defconst linkdb--tables
  '("CREATE TABLE links (
       link_id INTEGER PRIMARY KEY,
       link_name TEXT NOT NULL,
       link_ref TEXT NOT NULL,
       link_type TEXT CHECK(link_type IN ('href', 'file')) NOT NULL DEFAULT 'href',
       notes TEXT)"
    
    "CREATE TABLE tags (
       tag_id INTEGER PRIMARY KEY,
       tag_name TEXT NOT NULL UNIQUE)"
    
    "CREATE TABLE taggings (
       link_id INTEGER NOT NULL,
       tag_id INTEGER NOT NULL,
       FOREIGN KEY (link_id)
         REFERENCES links (link_id) ON DELETE CASCADE ON UPDATE CASCADE,
       FOREIGN KEY (tag_id)
         REFERENCES tags (tag_id) ON DELETE CASCADE ON UPDATE CASCADE)")
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
        (dolist (tab linkdb--tables)
          (sqlite-execute linkdb--db tab))))))

(provide 'linkdb)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; linkdb.el ends here
