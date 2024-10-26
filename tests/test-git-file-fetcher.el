;; -*- lexical-binding: t; -*-

(require 'git-file-fetcher)
(require 'vc)


(describe "GitHub info parser"
	  :var ((repo "github:vkhashimoto/dotfiles"))

	  (it "can get GitHub's info (host, url, repo)"
	      (expect (ff/--get-provider-info repo)
		      :to-equal
		      '("github.com" "https://github.com/" "vkhashimoto/dotfiles")))
	  (it "can get GitHub's name"
	      (expect (ff/--get-provider-name repo)
		      :to-equal
		      "github"))
	  (it "can get GitHub's directory"
	      (expect (ff/--get-directory repo)
		      :to-equal
		      "./tmp/github.com/vkhashimoto/dotfiles"
		      )))

(describe "Custom git host info parser"
	  :var (parser (repo "vkhashimoto:vkhashimoto/dotfiles"))
	  (before-all
	   (defun custom-parser (repo)
	     (let* ((host "git.vkhashimoto.dev")
		    (url "https://git.vkhashimoto.dev/")
		    (repo (nth 1 (split-string repo ":"))))
	       (list host url repo)))
	   (setf parser 'custom-parser)
	   (ff/add-git-provider "vkhashimoto" parser))

	  (it "can get host, url, repo"
	      (expect (ff/--get-provider-info repo)
		      :to-equal
		      '("git.vkhashimoto.dev" "https://git.vkhashimoto.dev/" "vkhashimoto/dotfiles")))
	  (it "can get git's name"
	      (expect (ff/--get-provider-name repo)
		      :to-equal
		      "vkhashimoto"))
	  (it "can get git's directory"
	      (expect (ff/--get-directory repo)
		      :to-equal
		      "./tmp/git.vkhashimoto.dev/vkhashimoto/dotfiles")))


(describe "repo-line parser"
	  (it "can parse repo, file, lines and rev"
	      (expect (ff/--parse-line ":repo \"github:vkhashimoto/dotfiles\" :file \".bashrc\" :lines \"1,7\" :rev \"develop\"")
		      :to-equal
		      '("github:vkhashimoto/dotfiles" ".bashrc" "1,7" "develop")))
	  (it "can parse repo, file and lines"
	      (expect (ff/--parse-line ":repo \"github:vkhashimoto/dotfiles\" :file \".bashrc\" :lines \"1,7\"")
		      :to-equal
		      '("github:vkhashimoto/dotfiles" ".bashrc" "1,7"))))

(describe "A remote git repository"
	  :var* ((repo "local:vkhashimoto/dotfiles") (current-dir default-directory) repo-directory)
	  (before-all
	   (defun custom-parser (repository)
	     (let* ((host "local")
		    (url (format "%s" "tmp/remote"))
		    (repository (nth 1 (split-string repository ":"))))
	       (list host url repository)))
	   (setf parser 'custom-parser)
	   (ff/add-git-provider "local" parser)
	   (setq repo-directory (ff/--get-directory repo))
	   (let ((remote-directory "./tmp/remote"))
	     (vc-create-repo 'Git)
	     (make-directory "./tmp/remote" t)
	     (let ((default-directory (format "%s%s" default-directory remote-directory)))
		 (vc-git-command nil 0 nil "init" "--bare" "vkhashimoto/dotfiles"))))
	  (after-all
	   (delete-directory "./tmp" 'recursive))
	  (it "can be cloned"
	      (ff/--clone repo)
	      (let* ((default-directory (format "%s%s" default-directory repo-directory))
		    (remote-url (with-temp-buffer
				  (vc-git-command (current-buffer) 0 nil "config" "--get" "remote.origin.url")
				  (buffer-string))))
		(expect (string-suffix-p (format "/tmp/remote/%s" (nth 2 (ff/--get-provider-info repo))) (string-trim remote-url))
			      :to-be
			      t))))
		    
(describe "A local git repository"
	  :var* (latest-commit (repo "github:vkhashimoto/dotfiles") (current-dir default-directory) (repo-directory (ff/--get-directory repo)))
	  (before-all
	   (make-directory repo-directory t)

	   (let ((default-directory (format "%s%s" default-directory repo-directory)))
		 (vc-create-repo 'Git)
		 (with-temp-file "file.txt"
		   (insert "Hello World"))
		 (vc-git-command nil 0 "file.txt" "add")
		 (vc-git-command nil 0 nil "commit" "-m" "First commit")
		 (setq latest-commit (vc-git--rev-parse "HEAD"))))
	  (after-all
	   (delete-directory "./tmp" 'recursive))

	   
	  (it "can get the latest commit"
	      (expect (ff/--get-latest-commit repo)
		      :to-equal
		      latest-commit))
	  (it "can change branches"
	      (let ((default-directory (format "%s%s" default-directory repo-directory)))
		    (vc-git-command nil 0 nil  "checkout" "-b" "develop"))
	      (ff/--checkout repo "master")
	      (let ((default-directory (format "%s%s" default-directory repo-directory)))
		    (let ((branch (with-temp-buffer
			       (vc-git-command (current-buffer) 0 nil "rev-parse" "--abbrev-ref" "HEAD")
			       (buffer-string))))
		      (expect (string-trim branch)
			      :to-equal
			       "master")))))

(describe "A buffer"
	  :var* (latest-commit (repo "local:vkhashimoto/text-content") (current-dir default-directory) repo-directory)
	  (before-all
	   (defun custom-parser (repository)
	     (let* ((host "local")
		    (url (format "%s" "tmp/remote"))
		    (repository (nth 1 (split-string repository ":"))))
	       (list host url repository)))
	   (setf parser 'custom-parser)
	   (ff/add-git-provider "local" parser)
	   (setq repo-directory (ff/--get-directory repo))
	   (let ((remote-directory "./tmp/remote"))
	     (vc-create-repo 'Git)
	     (make-directory "./tmp/remote" t)
	     (let ((default-directory (format "%s%s" default-directory remote-directory)))
		 (vc-git-command nil 0 nil "init" "--bare" "vkhashimoto/text-content")))
	   (ff/--clone repo)
	   (let ((default-directory (format "%s%s" default-directory repo-directory)))
	     (with-temp-file "file.txt"
	       (insert "Hello World")
	       (newline)
	       (insert "How are you?")
	       (newline)
	       (insert "Goodbye World"))
	     (vc-git-command nil 0 "file.txt" "add")
	     (vc-git-command nil 0 nil "commit" "-m" "First commit")
	     (setq latest-commit (vc-git--rev-parse "HEAD"))
	     (vc-git-push)))
	  (after-all
	   (delete-directory "./tmp" 'recursive))

	  (it "can get file content"
	      (let ((content (ff/get-content :repo "local:vkhashimoto/text-content" :file "file.txt" :lines "1,1" :rev nil)))
		(expect content :to-equal "Hello World")))
	  (it "can get file content from line"
	      (let ((content (with-temp-buffer
			       (insert "# ff: :repo \"local:vkhashimoto/text-content\" :file \"file.txt\" :lines \"1,2\"")
			       (goto-char (point-min))
			       (ff/get-content-from-line))))
		(expect content :to-equal (format "%s\n%s" "Hello World" "How are you?"))))
	  (it "can replace a source block with content from line"
	      (let ((content (with-temp-buffer
			       (insert "# ff: :repo \"local:vkhashimoto/text-content\" :file \"file.txt\" :lines \"1,3\"")
			       (newline)
			       (insert "#+begin_src blah")
			       (newline)
			       (insert "#+end_src")
			       (goto-char (point-min))
			       (ff/replace-src-block-from-line)
			       (buffer-string))))
		(expect content :to-equal 
(format "# ff: :repo \"local:vkhashimoto/text-content\" :file \"file.txt\" :lines \"1,3\" :rev \"%s\"
#+begin_src blah
Hello World
How are you?
Goodbye World
#+end_src" latest-commit)))))
