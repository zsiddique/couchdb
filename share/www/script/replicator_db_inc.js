var replicator_db = {};
replicator_db.wait_rep_doc = 500; // number of millisecs to wait after saving a Rep Doc
replicator_db.dbA = new CouchDB("test_suite_rep_db_a", {"X-Couch-Full-Commit":"false"});
replicator_db.dbB = new CouchDB("test_suite_rep_db_b", {"X-Couch-Full-Commit":"false"});
replicator_db.repDb = new CouchDB("test_suite_rep_db", {"X-Couch-Full-Commit":"false"});
replicator_db.usersDb = new CouchDB("test_suite_auth", {"X-Couch-Full-Commit":"false"});

replicator_db.docs1 = [
  {
    _id: "foo1",
    value: 11
  },
  {
    _id: "foo2",
    value: 22
  },
  {
    _id: "foo3",
    value: 33
  }
];

replicator_db.waitForRep = function waitForSeq(repDb, repDoc, state) {
  var newRep,
      t0 = new Date(),
      t1,
      ms = 3000;

  do {
    newRep = repDb.open(repDoc._id);
    t1 = new Date();
  } while (((t1 - t0) <= ms) && newRep._replication_state !== state);
}

replicator_db.waitForSeq = function waitForSeq(sourceDb, targetDb) {
  var targetSeq,
      sourceSeq = sourceDb.info().update_seq,
      t0 = new Date(),
      t1,
      ms = 3000;

  do {
    targetSeq = targetDb.info().update_seq;
    t1 = new Date();
  } while (((t1 - t0) <= ms) && targetSeq < sourceSeq);
}

replicator_db.waitForDocPos = function waitForDocPos(db, docId, pos) {
  var doc, curPos, t0, t1,
      maxWait = 3000;

  doc = db.open(docId);
  curPos = Number(doc._rev.split("-", 1));
  t0 = t1 = new Date();

  while ((curPos < pos) && ((t1 - t0) <= maxWait)) {
     doc = db.open(docId);
     curPos = Number(doc._rev.split("-", 1));
     t1 = new Date();
  }

  return doc;
}

replicator_db.wait = function wait(ms) {
  var t0 = new Date(), t1;
  do {
    CouchDB.request("GET", "/");
    t1 = new Date();
  } while ((t1 - t0) <= ms);
}


replicator_db.populate_db = function populate_db(db, docs) {
  if (db.name !== replicator_db.usersDb.name) {
    db.deleteDb();
    db.createDb();
  }
  for (var i = 0; i < docs.length; i++) {
    var d = docs[i];
    delete d._rev;
    T(db.save(d).ok);
  }
}