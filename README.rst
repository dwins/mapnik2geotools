what's this?
------------

A small scala project with the goal of transforming
`Mapnik <http://mapnik.org/>`_ map definitions to
something `GeoTools <http://geotools.org/>`_ and
`GeoServer <http://geoserver.org/>`_ can deal with.

setup
-----

1. Install SBT. See the instructions at http://simple-build-tool.googlecode.com/

2. ``sbt "run path/to/file.xml"`` runs the conversion process on a mapnik xml
   file.  The result is dumped to a file with the same name, but with the
   ``.xml`` extension replaced with ``.sld``.  (Watch out, it doesn't check
   whether the destination file exists!)

bugs/feature requests
---------------------
many. let me know what you find :)
