require_relative '../lib/manifest'
require_relative '../lib/s3'
require_relative 'task'

class DeployAssetsTask < Task
  def run(manifest, repo_dir)
    s3_web_site = manifest.s3_web_site
    bucket_name = s3_web_site.bucket
    values = {
      bucket: bucket_name
    }
    bucket_policy = transform(s3_web_site.bucket_policy.to_hash) do |v|
      v.is_a?(String) ? v % values : v
    end
  
    index_document = s3_web_site.index_document
    error_document = s3_web_site.error_document
    files_dir = s3_web_site.files_dir

    trace 'Creating S3 bucket' do
      S3.create_bucket bucket_name
    end

    trace 'Putting bucket policy' do
      S3.put_bucket_policy bucket_name, bucket_policy
    end

    trace 'Configuring S3 web site' do
      S3.website bucket_name, index_document, error_document
    end

    trace 'Putting objects' do
      s3_web_site.files.each do |file|
        local_path = File.expand_path(File.join(files_dir, file.local_path), manifest.dir)
        trace "Put object #{file.key}" do
          S3.put_object bucket_name, file.key, local_path, file.content_type
        end
      end
    end
  end

  private

  def transform(obj, &block)
    if obj.is_a?(Hash)
      temp = {}
      obj.each do |k, v|
        temp[transform(k, &block)] = transform(v, &block)
      end
      temp
    elsif obj.is_a?(Array)
      obj.map { |item| transform(item, &block) }
    else
      block.call(obj)
    end
  end
end
