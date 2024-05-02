#!/bin/bash

set -e

# A script for listing all known information about a given user,
# handy for checking what's going to happen in the case of deleting a user.

user_id='<user_id>'

run_query() {
  local query=$1
  echo "${query}" | echo "ADD YOUR psql COMMAND HERE"
}


echo "User info:"
echo "---------"
run_query "SELECT handle, name FROM users WHERE id = :'user_id';"
echo "---------"
echo ""
echo "User projects:"
echo "---------"
run_query "SELECT slug, private FROM projects WHERE owner_user_id = :'user_id';"
echo "---------"
echo ""
echo "Contributor branches:"
run_query "SELECT owner.handle as project_owner, p.slug as slug, pb.name
             FROM projects p
             JOIN project_branches pb ON p.id = pb.project_id
             JOIN users owner ON p.owner_user_id = owner.id
             WHERE pb.contributor_id = :'user_id';
"
echo "---------"
echo ""
echo "Contributions:"
echo "---------"
run_query "SELECT c.title, c.description FROM contributions c WHERE c.author_id = :'user_id';"
echo "---------"
echo ""
echo "Tickets"
echo "---------"
run_query "SELECT t.title, t.description FROM tickets t WHERE t.author_id = :'user_id';"
echo "---------"
echo ""
echo "Cloud Environments"
echo "---------"
run_query "SELECT e.name FROM environments e WHERE e.user_id = :'user_id';"
echo "---------"
echo ""
echo "Cloud Services"
echo "---------"
run_query "SELECT s.service_hash, ns.service_name
             FROM services s
             JOIN named_services ns ON s.service_hash = ns.service_hash
             WHERE s.user_id = :'user_id';
"
echo "---------"
echo ""
echo "Cloud Web Services"
echo "---------"
run_query "SELECT ws.service_hash
             FROM web_services ws
             WHERE ws.user_id = :'user_id';
"
