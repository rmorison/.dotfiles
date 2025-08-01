## Development Workflow
- The Emacs init is in .emacs.d/Emacs.org and needs to be tangled into .emacs.d/init.el after editing.
- On macOS, the Emacs binary is located at `/Applications/Emacs.app/Contents/MacOS/Emacs`
- To tangle Emacs.org: `cd .emacs.d && /Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "Emacs.org")'`