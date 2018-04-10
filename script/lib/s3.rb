require 'JSON'
require 'tempfile'
require_relative 'shell'

module S3
  def self.create_bucket(bucket_name)
    Shell.check_run(
      'aws',
      's3api',
      'create-bucket',
      '--bucket', bucket_name)
  end

  def self.put_bucket_policy(bucket_name, bucket_policy)
    f = Tempfile.new
    begin
      policy_json_path = f.path
      f.close

      File.write policy_json_path, JSON.pretty_generate(bucket_policy)

      Shell.check_run(
        'aws',
        's3api',
        'put-bucket-policy',
        '--bucket', bucket_name,
        '--policy', 'file://' + policy_json_path)
    ensure
      f.close
      f.unlink
    end
  end

  def self.put_object(bucket_name, key, path, content_type)
    Shell.check_run(
      'aws',
      's3api',
      'put-object',
      '--bucket', bucket_name,
      '--key', key,
      '--body', path,
      '--content-type', content_type)
  end

  def self.website(bucket_name, index_document, error_document)
    Shell.check_run(
      'aws',
      's3',
      'website',
      "s3://#{bucket_name}",
      '--index-document', index_document,
      '--error-document', error_document)
  end
end
