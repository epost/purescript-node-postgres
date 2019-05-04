#+title: PureScript bindings for node-postgres

#+BEGIN_HTML
<a href="http://pursuit.purescript.org/packages/purescript-node-postgres">
  <img src="http://pursuit.purescript.org/packages/purescript-node-postgres/badge" alt="purescript-node-postgres on Pursuit">
  </img>
</a>
<br>
#+END_HTML


PureScript bindings for the [[https://www.npmjs.org/package/pg][pg library]] ([[https://github.com/brianc/node-postgres][node-postgres]] on GitHub).

* Installation

Clone the project and install its dependencies:

#+begin_src bash
git clone https://github.com/epost/purescript-node-postgres
cd purescript-node-postgres
npm install
#+end_src

* Building

Build:

#+begin_src bash
npm run build
#+end_src

* Testing

Assuming you have [[http://www.postgresql.org/][PostgreSQL]] installed, create a database with some test data:

#+begin_src bash
psql --command="create database test"
psql --command="create user testuser"
psql --username=testuser test -f schema.sql
#+end_src

Then run the tests:

#+begin_src bash
npm run test
#+end_src

* Documentation

See [[https://pursuit.purescript.org/packages/purescript-node-postgres/5.0.0][Module documentation]].
