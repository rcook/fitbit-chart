@echo off
aws --endpoint-url=http://localhost:4569 dynamodb create-table --table-name weight-samples --attribute-definitions AttributeName=date,AttributeType=S --key-schema AttributeName=date,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5
