var request = require('superagent')

var host  = process.argv[2] || 'localhost'
var port  = process.argv[3] || '9200'
var index = process.argv[4] || 'clippy'

request
  .del("http://" + host + ":" + port + "/" + index)
  .end(function(err, res){
    if(err){
      console.log(err)
    }
    console.log('Success!')
  })
