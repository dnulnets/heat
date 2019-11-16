#!/bin/sh
REPO_URL=localhost:32000
curl -k -s -X GET http://$REPO_URL/v2/_catalog | jq '.repositories[]' | sort | xargs -I _ curl -s -k -X GET http://$REPO_URL/v2/_/tags/list
