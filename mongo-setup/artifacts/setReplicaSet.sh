./mongo --host 127.0.0.1:27017 << 'EOF'
config = { _id: "rs1", members:[
          { _id : 0, host : "dtldevtt147.fr.world.socgen:27017"},
          { _id : 1, host : "dtldevtt147.fr.world.socgen:27018", "arbiterOnly" : true},
          { _id : 2, host : "dtldevtt230.fr.world.socgen:27017"} ]
         };
rs.initiate(config);

rs.status();
EOF


rs.addArb("dtldevtt147.fr.world.socgen:27017")


mongodump --dbpath /home/dtldev07/homeware/mongo/data -d soa -o ~/homeware/bkp