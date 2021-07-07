ggplotnim overview
==================

The ``ggplotnim`` packages is actually rather two packages in one.

#. a plotting library following
   `ggplot2's <https://ggplot2.tidyverse.org/>`__ syntax
#. a dataframe library mostly following
   `dplyr <https://dplyr.tidyverse.org/>`__

It may or may not be split into two packages at some point.

The dataframe library was a necessity achieve a ggplot2 like syntax and
provide ways to manipulate the data in the desired fashion.

For successful usage three major things have to be known:

-  the ``DataFrame`` and the procs working on it (**see:**
   `formula.nim <./formula.html>`__ documentation!)
-  the ``f{}`` macro to create ``FormulaNodes``, which are needed for
   the DF procs
-  the ``ggplot`` syntax

Creating a DF
-------------

Usage typically starts with one of the following cases:

#. data already available in ``seq[T]`` or some Nim object from which
   such can be created
#. some CSV / TSV like ascii file
#. some binary file like HDF5
#. some database

Note about 3 and 4: simple access (without manually reading into a
``seq[T]``) is not supported for these two yet. If there's demand for
either of the two please open an issue. It should be easy to add some
helper procs.

From ``seq[T]``
~~~~~~~~~~~~~~~

For the case of having the data as ``seq[T]``, we just use the
``seqsToDf`` proc to create a DF from it. There are two ways to use it.
Assuming we have three sequences of possibly different types:

.. code:: nim

   let s1 = @[22, 54, 34]
   let s2: seq[float] = @[1.87, 1.75, 1.78]
   let s3: seq[string] = @["Mike", "Laura", "Sue"]

we can either create a DF and let the library automatically deduce the
column names:

.. code:: nim

   let dfAutoNamed = seqsToDf(s1, s2, s3)

which will give us a DF with column names:

.. code:: nim

   "s1", "s2", "s3"

that is the idenfitier of the ``seq[T]`` is stringified. In many cases
one might rather like a different name. In this case use the following
syntax:

.. code:: nim

   let df = seqsToDf({ "Age" : s1,
                       "Height" : s2,
                       "Name" : s3 })

which will then use the given strings for the column names.

From a CSV / TSV file
~~~~~~~~~~~~~~~~~~~~~

The second supported case is a CSV like file. For these the library
provides a generalized ``readCsv`` proc. Strictly speaking it can also
read TSV (or any delimited ascii file) and allows to skip N lines at the
beginning.

.. code:: nim

   proc readCsv*(fname: string,
                 sep = ',',
                 header = "#",
                 skipLines = 0): OrderedTable[string, seq[string]]

Note that the header argument is only used to remove the header
delimiter from the first line in the file. The header is **always** the
first line! Also note that it returns not a DF, but an OrderedTable of
string values only. To get a DF from this, we have to call ``toDf`` on
the result. Let's use it to read the provided ``mpg`` dataset:

.. code:: nim

   let df = toDf(readCsv("../data/mpg.csv"))

The ``toDf`` will try to determine the types of the columns. It first
assumes something might be a number and an integer and starts with
``parseInt``. If that fails, ``parseFloat`` is attempted. If that fails
too, the element (and rest of the column) is parsed as a string. This
also means that at the moment it's not possible to parse ``bool`` values
as ``VBool`` from an ascii file! The downsides of this approach are
visible e.g. when reading the ``msleep`` dataset. Since it contains many
missing "NA", the columns which contain those will store the other float
values as strings (this might be changed soon; at the moment every
column is an "object column" in pandas terms anyways).

Manipulating a DF
-----------------

Now we have a DF. What then?

First of all we can look at it. Echoing a DF calls the ``pretty`` proc.
For the DF introduced above, this looks like:

.. code:: nim

   echo df

gives for the ``mpg`` dataset:

.. code:: bash

   Dataframe with 11 columns and 234 rows:
    Idx    manufacturer           model           displ  ...  cyl  ...  drv   cty   hwy   fl     class
      0            audi              a4             1.8  ...    4  ...    f    18    29    p   compact
      1            audi              a4             1.8  ...    4  ...    f    21    29    p   compact
      2            audi              a4               2  ...    4  ...    f    20    31    p   compact
      3            audi              a4               2  ...    4  ...    f    21    30    p   compact
      4            audi              a4             2.8  ...    6  ...    f    16    26    p   compact
      5            audi              a4             2.8  ...    6  ...    f    18    26    p   compact
      6            audi              a4             3.1  ...    6  ...    f    18    27    p   compact
      7            audi      a4 quattro             1.8  ...    4  ...  "4"    18    26    p   compact
      8            audi      a4 quattro             1.8  ...    4  ...  "4"    16    25    p   compact
      9            audi      a4 quattro               2  ...    4  ...  "4"    20    28    p   compact
     10            audi      a4 quattro               2  ...    4  ...  "4"    19    27    p   compact
     11            audi      a4 quattro             2.8  ...    6  ...  "4"    15    25    p   compact
     12            audi      a4 quattro             2.8  ...    6  ...  "4"    17    25    p   compact
     13            audi      a4 quattro             3.1  ...    6  ...  "4"    17    25    p   compact
     14            audi      a4 quattro             3.1  ...    6  ...  "4"    15    25    p   compact
     15            audi      a6 quattro             2.8  ...    6  ...  "4"    15    24    p   midsize
     16            audi      a6 quattro             3.1  ...    6  ...  "4"    17    25    p   midsize
     17            audi      a6 quattro             4.2  ...    8  ...  "4"    16    23    p   midsize
     18       chevrolet c1500 suburb...             5.3  ...    8  ...    r    14    20    r       suv
     19       chevrolet c1500 suburb...             5.3  ...    8  ...    r    11    15    e       suv

(NOTE: I shortened the output for the docs here) Notice how in the
``drv`` column the 4WD entries are echoed as "4" instead of just 4. That
is to highlight that those values are actually stored as ``VString``.

By default only the first 20 entries will be shown. For more/less
elements, call ``pretty`` directly:

.. code:: nim

   echo df.pretty(100)

``pretty`` also takes a ``precision`` argument. This is given to the
string conversion for ``VFloat`` values to set the number of digits
printed after the decimal point. However, it can also be used to change
the width of the columns more generally. Note however the precision is
added to a width of ``6`` by default. Also the column is at least as
wide as the longest DF key.

Let's now check which cars in the dataset have the highest and lowest
city fuel economy. For that we can simply arrange the dataframe
according to the ``cty`` column and take the tail or head of the result.

.. code:: nim

   echo df.arrange("cty").head(5)

results in:

.. code:: bash

   Dataframe with 11 columns and 5 rows:
   Idx    manufacturer           model           displ  ...  cyl  ...  drv   cty   hwy  fl    class
     0           dodge dakota picku...             4.7  ...    8  ...  "4"     9    12   e   pickup
     1           dodge     durango 4wd             4.7  ...    8  ...  "4"     9    12   e      suv
     2           dodge ram 1500 pic...             4.7  ...    8  ...  "4"     9    12   e   pickup
     3           dodge ram 1500 pic...             4.7  ...    8  ...  "4"     9    12   e   pickup
     4            jeep grand cherok...             4.7  ...    8  ...  "4"     9    12   e      suv

and looking at the tail instead:

.. code:: nim

   echo df.arrange("cty").tail(5)

will tell us that a new beetle is the most efficient car in the dataset:

.. code:: bash

   Dataframe with 11 columns and 5 rows:
   Idx    manufacturer           model           displ  ...  cyl  ...  drv   cty   hwy   fl        class
     0           honda           civic             1.6  ...    4  ...    f    28    33    r   subcompact
     1          toyota         corolla             1.8  ...    4  ...    f    28    37    r      compact
     2      volkswagen      new beetle             1.9  ...    4  ...    f    29    41    d   subcompact
     3      volkswagen           jetta             1.9  ...    4  ...    f    33    44    d      compact
     4      volkswagen      new beetle             1.9  ...    4  ...    f    35    44    d   subcompact

(``arrange`` also takes an order argument, using the stdlib's
``SortOrder`` enum).

As another example here to showcase the usage of ``FormulaNodes``, let's
find some cars with an engine displacement of more than 5 L and which
are 2 seaters (I wonder what car might show up…):

.. code:: nim

   echo df.filter(f{"displ" > 5.0 and "class" == "2seater"})

.. code:: bash

   Dataframe with 11 columns and 5 rows:
   Idx    manufacturer           model           displ  ...  cyl  ...  drv   cty   hwy   fl     class
     0       chevrolet        corvette             5.7  ...    8  ...    r    16    26    p   2seater
     1       chevrolet        corvette             5.7  ...    8  ...    r    15    23    p   2seater
     2       chevrolet        corvette             6.2  ...    8  ...    r    16    26    p   2seater
     3       chevrolet        corvette             6.2  ...    8  ...    r    15    25    p   2seater
     4       chevrolet        corvette               7  ...    8  ...    r    15    24    p   2seater

Surprise, surprise we found ourselves a bunch of corvettes!

Finally, let's make use of a formula, which takes an assignment. Let's
say we want to convert the city fuel economy of the cars from MPG to
L/100 km as is the standard in Germany. We'll do this with ``mutate``.
``mutate`` will add an additional column to the dataframe. (well, if
only it was clear whether the ``mpg`` given are US gallon or imperial
gallon?)

.. code:: nim

   let dfl100km = df.filter(f{"displ" > 5.0 and "class" == "2seater"})
     .mutate(f{"cty / L/100km" ~ 235 / "cty"})
   echo dfl100km.pretty(5)

shows us:

.. code:: bash

   Dataframe with 12 columns and 5 rows:
   Idx     manufacturer            model            displ  ...       trans  ...  cty   ...   cty / L/100km
     0        chevrolet         corvette              5.7  ...  manual(m6)  ...   16   ...           14.69
     1        chevrolet         corvette              5.7  ...    auto(l4)  ...   15   ...           15.67
     2        chevrolet         corvette              6.2  ...  manual(m6)  ...   16   ...           14.69
     3        chevrolet         corvette              6.2  ...    auto(s6)  ...   15   ...           15.67
     4        chevrolet         corvette                7  ...  manual(m6)  ...   15   ...           15.67

where I removed a couple of columns for better visibility.

I used the chaining of ``filter`` and ``mutate`` above mainly to
showcase that this works reliably. However, there's no magic happening
to optimize any chaining!

When looking at the formula above note that as in ggplot2 the tilde ~ is
used to indicate a dependency.

Finally it should be mentioned that it's possible to also call procs in
the usage of formulas. Two kind of procs are supported. Either a proc
takes a ``seq[T]`` and returns a ``T``, or it takes a ``T`` and returns
a ``T``. These have to be lifted to work with
``PersistentVector[Value]``. Helper templates to lift normal procs are
provided. See formula.nim and check for ``lift<X><Y>Proc``, where ``X``
of ``{Scalar, Vector}`` and ``Y`` of ``{Int, Float}``.
