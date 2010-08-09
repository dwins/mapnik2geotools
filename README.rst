what's this?
------------

A small scala project with the goal of transforming
`Mapnik <http://mapnik.org/>`_ map definitions to
something `GeoTools <http://geotools.org/>`_ and
`GeoServer <http://geoserver.org/>`_ can deal with.

setup
-----

1. Install SBT. See the instructions at http://simple-build-tool.googlecode.com/

2. ``sbt update`` to fetch dependencies
 
3. ``sbt compile`` to build everything

usage
-----

There's currently no standalone launcher script, so the recommended usage is to
start up the SBT prompt by running ``sbt`` with no arguments.  You can then use
the ``run`` task to execute the converter.  Currently, you have to run it twice
to get GeoServer fully setup:

1. First, generate files on disk by running with the name of a mapnik map
   definition::

      run /path/to/mapnik/map.xml

   This creates one SLD file in ``/path/to/mapnik/output/`` for each ``Style``
   element in the mapfile.  You can inspect these manually (when uploading to
   GeoServer through REST the styles will be rewritten.)  There is also a
   ``{dbname}.sql`` file ({dbname} will be substituted for the name of the
   database) which creates views for each datastore that uses a subquery.  If
   you use Postgis, you should execute this SQL against your database to set up
   those aliases for GeoServer (as GeoServer does not currently support
   subqueries in datastore definitions).  This looks like::

      psql -d mydb -f /path/to/mapnik/output/mydb.sql

2. With the tables and views in place, run the converter with a GeoServer REST
   URL and datadir::

      run /path/to/mapnik/map.xml --rest http://localhost:8080/geoserver/rest --datadir /path/to/geoserver_data
   Optionally, you can also provide --user <geoserver user name> and --password
   <geoserver user password>.  If you omit these, the default admin:geoserver
   credentials will be assumed.

bugs/feature requests
---------------------
many. let me know what you find :)
