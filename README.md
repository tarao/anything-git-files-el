anything-git-files.el --- anything for git files
================================================

## Features

- List all the files in a git repository which the file of the current buffer belongs to
  - List untracked files and modified files in different sources
- List files in the submodules
- Cache the list as long as the repository is not modified
- Avoid interrupting user inputs; external git command is invoked asynchronously

## Install

### Manual Install

1. Install dependent package [anything.el](http://www.emacswiki.org/Anything)
2. Locate [anything-git-files.el](https://raw.github.com/tarao/anything-git-files-el/master/anything-git-files.el) in your load path

### Install by [MELPA](http://melpa.milkbox.net/)

1. Type ``M-x package-install RET anything-git-files RET``.

### Install by el-get

1. Save the lines below as anything-git-files.rcp in your recipe directory
2. Type ``M-x el-get RET anything-git-files RET``

```lisp
(:name anything-git-files
       :type github
       :pkgname "tarao/anything-git-files-el"
       :depends anything)
```

## Usage

Load anything-git-files.el in your ~/.emacs or ~/emacs.d/init.el:
```lisp
(require 'anything-git-files)
```

Then, ``M-x anything-git-files`` will list the files in a git
repository.  Note that ``M-x anything-git-files`` will fail when the
file of the current buffer is not in a git repository.

## Customization

### Hack the anything sources

The anything sources to get files in a git repository are
``anything-git-files:modified-source``,
``anything-git-files:untracked-source`` and
``anything-git-files:all-source``.

The list of anything sources for submodules can be retrieved by
function ``anything-git-files:submodule-sources``. The function takes
one argument, which is a list of symbols of source type, ``modified``,
``untracked`` or ``all``. For example,
``(anything-git-files:submodule-sources '(untracked all)`` returns
anything sources for untracked files and all files in the git
repository of the submodules.

The following example defines a custom anything function to
list files from several sources, including ones from
anything-git-files.el.
```lisp
(defun tarao/anything-for-files ()
  (interactive)
  (require 'anything-config)
  (require 'anything-git-files)
  (let* ((git-source (and (anything-git-files:git-p)
                          `(anything-git-files:modified-source
                            anything-git-files:untracked-source
                            anything-git-files:all-source
                            ,@(anything-git-files:submodule-sources 'all))))
         (other-source '(anything-c-source-recentf
                         anything-c-source-bookmarks
                         anything-c-source-files-in-current-dir+
                         anything-c-source-locate))
         (sources `(anything-c-source-buffers+
                    anything-c-source-ffap-line
                    anything-c-source-ffap-guesser
                    ,@git-source
                    ,@other-source)))
    (anything-other-buffer sources "*anything for files*")))
```

## See Also

- A guide in Japanese: [anythingでgitリポジトリ内のファイルの全列挙をきちんとやる](http://d.hatena.ne.jp/tarao/20130421/1366553578)
