NEWS
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor
  and patch)
* New additions without breaking backward compatibility bumps the minor
  (and resets the patch)
* Bug fixes and misc changes bumps the patch


wakefield 0.1.0
----------------------------------------------------------------

&lt;b&gt;BUG FIXES&lt;/b&gt;

&lt;b&gt;NEW FEATURES&lt;/b&gt;

* `r_list` & `r_data_frame` now add a suffix to repeat variable names in a
  sensible way.  The separator is controlled by `rep.sep`.  Suggested by
  Ananda Mahto.  See <a href="https://github.com/trinker/wakefield/issues/1">issue #1</a> for details.

&lt;b&gt;MINOR FEATURES&lt;/b&gt;

IMPROVEMENTS

&lt;b&gt;CHANGES&lt;/b&gt;


wakefield 0.0.1
----------------------------------------------------------------

This package is designed to generates random data sets including: `data.frames`,
    `lists`, and `vectors`.