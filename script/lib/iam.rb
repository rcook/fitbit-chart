require 'tempfile'
require_relative 'json_payload'
require_relative 'shell'

module IAM
  def self.create_role(role_name, policy_document, role_policy)
    status, output = JsonPayload.with_temp(policy_document) do |policy_document_url|
      Shell.capture(
        'aws',
        'iam',
        'create-role',
        '--role-name', role_name,
        '--assume-role-policy-document', policy_document_url)
    end

    unless status.success?
      return if output.include?('EntityAlreadyExists')
      raise "create-role command failed: output=[#{output}] status=[#{status}]"
    end

    JsonPayload.with_temp(role_policy) do |role_policy_url|
      Shell.check_run(
        'aws',
        'iam',
        'put-role-policy',
        '--role-name', role_name,
        '--policy-name', "Permissions-#{role_name}",
        '--policy-document', role_policy_url)
    end

    # Sleep until role and policies fully created and propagated
    sleep 5
  end
end
