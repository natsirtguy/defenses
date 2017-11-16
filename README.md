# defenses.el

Find defenses for fantasy football.

# Installation

Put `defenses.el` somewhere in your `load-path`.

# Dependencies

* [w3m](https://github.com/ecbrown/emacs-w3m)

# Usage

Calling `M-x defenses` will open a buffer with the defenses sorted by
their opponents' expected score. The expected scores are evaluated
using the over-unders and spreads from
(http://www.vegasinsider.com/nfl/odds/las-vegas/) and the spreads from
(https://projects.fivethirtyeight.com/2017-nfl-predictions/games/).

# Built Using
* [Cask](https://cask.readthedocs.io/en/latest/)
* [ert-runner.el](https://github.com/rejeep/ert-runner.el)
