Features
========

* Unlike newLISP's ``context``, ``Dict`` can store any arbitrary primitive
  data structures, including: ``int``, ``string``, ``symbol``, ``list``,
  ``function``, and even ``Dict`` itself.

* Use of ``nl-tim`` library's ``type-of`` to determine the type, named
  ``dict-t``.

Creating
========

To create an empty dictionary:

::

    (define a-dict (Dict:dict))

To create a dictionary with some pre-defined key-value pairs:

::

    (define a-dict (Dict:dict 'a 1 "b" 2 12 32 '(1 2 3) true))

Accessing
=========

::

    (define a-dict (Dict:dict 'a "Hello world"))
    (-> a-dict 'a)                                    => "Hello world"
    (-> a-dict 'b)                                    => nil

Modifying
=========

::

    (define a-dict (Dict:dict 'ten "10"))
    (-> a-dict 'ten)                                  => "10"
    (<- a-dict 'ten -10)
    (-> a-dict 'ten)                                  => -10
    (<- a-dict 'ten 10 'twenty 20)
    (-> a-dict 'ten)                                  => 10
    (-> a-dict 'twenty)                               => 20
    (setf a-dict 'twenty -20)
    (-> a-dict 'twenty)                               => -20

Converting
==========

To association list

::

    (define a-dict (Dict:dict 'a 1 "b" 2))
    (dict->list a-dict)                               => (('a 1) ("b" 2))
