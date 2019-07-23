# .emacs.d
Clojure focused init.el with good comments

Uses [straight.el](https://github.com/raxod502/straight.el) for deterministic package management. Versions are based off Git project SHAs. To upgrade dependencies, update the version-lock file at straight/versions/default.el and run `M-x straight-thaw-versions`. You can conversely run `M-x straight-freeze-versions` to overwrite with all version info of the currently installed packages.

Includes many Clojure tools including CIDER, font-locking, and more.

Relies on Projectile & Helm for project navigation (try `C-c P`) & dynamic autocompleting menus.

## License

Copyright © 2019 Quest Yarbrough

Distributed under the [WTFPL](http://www.wtfpl.net/)
