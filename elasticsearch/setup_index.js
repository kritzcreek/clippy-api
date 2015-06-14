var request = require('superagent')

var setupIndexQuery =
{
    "settings": {
        "analysis": {
            "filter": {
                "trigrams_filter": {
                    "type":     "ngram",
                    "min_gram": 3,
                    "max_gram": 3
                }
            },
            "analyzer": {
                "trigrams": {
                    "type":      "custom",
                    "tokenizer": "standard",
                    "filter":   [
                        "lowercase",
                        "trigrams_filter"
                    ]
                }
            }
        }
    },
    "mappings": {
        "yank": {
            "properties": {
                "contentType": {
                  "type": "string",
                  "analyzer": "trigrams"
                },
                "content": {
                    "type":     "string",
                    "analyzer": "trigrams"
                }
            }
        },
        "snippet": {
            "properties": {
                "snippetContent": {
                    "type": "string",
                    "analyzer": "trigrams"
                },
                "language": {
                    "type":     "string",
                    "analyzer": "trigrams"
                }
            }
        }
    }
}


var host  = process.argv[2] || 'localhost'
var port  = process.argv[3] || '9200'
var index = process.argv[4] || 'clippy'

request
  .put("http://" + host + ":" + (port ? port : "80") + "/" + index)
  .send(setupIndexQuery)
  .end(function(err, res){
    if(err){
      console.log(err)
    }
    console.log('Success!')
  })
