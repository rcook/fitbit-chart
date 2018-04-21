require_relative '../lib/iam'
require_relative '../lib/lambda'
require_relative '../lib/manifest'
require_relative '../lib/sts'
require_relative 'task'

class UpdateFunctionTask < Task
  def run(manifest, repo_dir, package_path)
    trace 'UpdateFunctionTask' do
      run_helper manifest, repo_dir, package_path
    end
  end

  private

  def run_helper(manifest, repo_dir, package_path)
    lambda_package = manifest.lambda_package
    function_name = lambda_package.function_name
    runtime = lambda_package.runtime
    handler = lambda_package.handler

    account_id = trace 'Getting AWS account ID' do
      STS.get_account_id
    end

    role = "arn:aws:iam::#{account_id}:role/#{function_name}"

    trace 'Creating IAM role' do
      IAM.create_role function_name, lambda_package.policy_document.to_hash, lambda_package.role_policy.to_hash
    end

    trace 'Creating or updating Lambda function' do
      Lambda.create_function function_name, package_path, runtime, role, handler
    end
  end
end
