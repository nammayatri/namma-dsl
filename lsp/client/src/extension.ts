import * as path from 'path';
import { workspace, ExtensionContext, ConfigurationTarget, window } from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const serverModule = context.asAbsolutePath(
		path.join('server', 'out', 'server.js')
	);
	const config = workspace.getConfiguration();
	const newAssociations = {
        "*.nd": "yaml",
        "*.ndsl": "yaml",
		"*.yaml": "yaml",
		"*.nammadsl": "yaml"
    };
	config.update('files.associations', newAssociations, ConfigurationTarget.Global)
        .then(() => {
            window.showInformationMessage('Settings updated!');
        }),(error) => {
        	window.showErrorMessage(`Failed to update settings: ${error}`);
        };
	const serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
		}
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'yaml'}],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	client = new LanguageClient(
		'NammaDSL',
		'Namma DSL Language Server',
		serverOptions,
		clientOptions
	);
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
