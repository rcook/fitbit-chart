require_relative '../lib/iam'
require_relative '../lib/lambda'
require_relative '../lib/manifest'
require_relative '../lib/sts'

class UpdateFunctionTask
  def run(manifest, repo_dir, package_path)
    lambda_package = manifest.lambda_package
    function_name = lambda_package.function_name
    runtime = lambda_package.runtime
    account_id = STS.get_account_id
    handler = lambda_package.handler
    role = "arn:aws:iam::#{account_id}:role/#{function_name}"
    IAM.create_role function_name, lambda_package.policy_document.to_hash, lambda_package.role_policy.to_hash
    Lambda.create_function function_name, package_path, runtime, role, handler
  end
end
