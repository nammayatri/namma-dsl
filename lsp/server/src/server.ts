import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult
} from 'vscode-languageserver/node';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

const yaml = require('yaml');
const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;
let currDocument:TextDocument;
let diagnostics: Diagnostic[] = [];
let parsedYAML: any;
let defaultDataTypes =
	[	'Id',
		'Text',
		'Maybe',
		'HighPrecMoney',
		'Int',
		'Bool',
		'Float',
		'Double',
		'TimeOfDay',
		'UTCTime',
	]

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			completionProvider: {
				resolveProvider: true
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

interface NammaDSLSettings {
	maxNumberOfProblems: number;
}

const defaultSettings: NammaDSLSettings = { maxNumberOfProblems: 1000 };
let globalSettings: NammaDSLSettings = defaultSettings;

const documentSettings: Map<string, Thenable<NammaDSLSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		documentSettings.clear();
	} else {
		globalSettings = <NammaDSLSettings>(
			(change.settings.languageServerExample || defaultSettings)
		);
	}
	documents.all().forEach(validateDSL);
});

documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});

documents.onDidChangeContent(change => {
	diagnostics.length = 0;
	validateDSL(change.document);
	connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

function typeSplit(inputType: string): string[] {
	return inputType.split(/[\[\]() ]+/)?.filter(Boolean);
}

function validateStorage(){
	let imports = parsedYAML.get('imports') ?? [];
	let definedDataNames = parsedYAML?.contents?.items?.map((item:any) => item.key.value)?.filter((item:any) => item !== 'imports') ?? [];
	definedDataNames.forEach((dataName:any) => {
		parsedYAML.has(dataName)?validateEachStorageData(parsedYAML.get(dataName),imports,definedDataNames):null;
	});
}

function validateEachStorageData (data:any, imports:any, definedDataNames:any){
	let internalDefinedTypeName = data.get('types')?.items?.map((item:any) => item.key.value) ?? [];
	let fieldTypes = data.get('fields')	?.items?.map((item:any) => item.value) ?? [];
	fieldTypes.forEach((fieldType:any) => {
		validateStorageDataFieldType(fieldType,imports,internalDefinedTypeName,definedDataNames);
	});
}

function validateStorageDataFieldType (fieldType:any,imports:any,internalDefinedTypeName:any,definedDataNames:any) {
	typeSplit(fieldType.value).forEach((type:any) => {
		if (!defaultDataTypes.includes(type) && !imports.has(type) && !internalDefinedTypeName.includes(type) && !definedDataNames.includes(type)) {
			diagnostics.push({
				severity: DiagnosticSeverity.Error,
				range: {
					start: currDocument.positionAt(fieldType.range[0]),
					end: currDocument.positionAt(fieldType.range[1])
				},
				message: `Type ${type} is not defined or imported`,
				source: 'Namma DSL',
			});
		}
	});
}

function validateDSL(document: TextDocument) {
	currDocument = document;
	let isValidYaml = false;
	try {
		const content = document.getText();
		yaml.parse(content);
		parsedYAML = yaml.parseDocument(content,{lineCounter: true});
		isValidYaml = true;
	} catch (error:any) {
		diagnostics.push({
                severity: DiagnosticSeverity.Error,
                range: {
                    start: document.positionAt(error.pos[0]),
                    end: document.positionAt(error.pos[1])
                },
                message: error.message,
                source: 'Namma DSL',
            });
	}
	if (isValidYaml) {
		validateStorage();
	}
}

connection.onDidChangeWatchedFiles(_change => {
	connection.console.log('Change occurred in watched files');
});

connection.onCompletion(
	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		return [
			{
				label: 'Namma DSL',
				kind: CompletionItemKind.Text,
				data: 1
			},
			{
				label: 'Unknown',
				kind: CompletionItemKind.Text,
				data: 2
			}
		];
	}
);

connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		if (item.data === 1) {
			item.detail = 'Namma DSL details';
			item.documentation = 'Namma DSL documentation';
		} else if (item.data === 2) {
			item.detail = 'Unknown';
			item.documentation = 'Unknown details';
		}
		return item;
	}
);

documents.listen(connection);

connection.listen();
