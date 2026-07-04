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

import { defaultDataTypes, typeSplit } from '../../Utils';

let currDocument:TextDocument;
let diagnostics: Diagnostic[] = [];
let parsedYAML: any;


export function validateStorage(parsedYAMLLoc: any, document : TextDocument):Diagnostic[]{
	diagnostics = [];
	parsedYAML = parsedYAMLLoc;
	currDocument = document;
	validateFieldDataTypes();
	return diagnostics;
}

function validateInternalDefinedTypes(data:any,imports:any,internalDefinedTypeName:any,definedDataNames:any) {
	let internalDefinedTypes = data.get('types')?.items ?? []
	internalDefinedTypes.forEach((internalDefinedType:any) => {
		// check if type is enum or not
		if (internalDefinedType.value.has('enum')) {
			if (internalDefinedType.value.items.length > (internalDefinedType.value.has('derive')?2:1)) {
				diagnostics.push({
					severity: DiagnosticSeverity.Error,
					range: {
						start: currDocument.positionAt(internalDefinedType.key.range[0]),
						end: currDocument.positionAt(internalDefinedType.value.range[1])
					},
					message: `In fields of ${internalDefinedType.key.value} enum is a keyword which is not allowed. If it's a Enum then please remove other fields apart from derive`,
					source: 'Namma DSL',
				});
		    } else {
				// now we know its a correct enum
				//TODO:Make better type location resolver.
				let enumTypeStartRange = internalDefinedType.value.items[0].value.range[0];
				let enumTypeEndRange = internalDefinedType.value.items[0].value.range[1];
				((internalDefinedType?.value?.items?.[0]?.value?.value)?.split(',') ?? []).forEach((enumValue:any) => {
					let enumTypes:string[] = enumValue.trim().split(' ');
					enumTypes.shift();
					enumTypes.forEach((enumType:string) => {
						let partialTypes = typeSplit(enumType);
						partialTypes.forEach((partialType:any) => {
							if (!partialType[0].includes('.') && !defaultDataTypes.includes(partialType[0]) && !imports.has(partialType[0]) && !internalDefinedTypeName.includes(partialType[0]) && !definedDataNames.includes(partialType[0])) {
								diagnostics.push({
									severity: DiagnosticSeverity.Error,
									range: {
										start: currDocument.positionAt(enumTypeStartRange),
										end: currDocument.positionAt(enumTypeEndRange)
									},
									message: `Type ${partialType[0]} is not defined or imported`,
									source: 'Namma DSL',
								});
							}
						});
					});
				});


			}
		} else {
				// here its not a enum
				internalDefinedType.value.items.forEach((fieldsWithType:any) => {
					typeSplit(fieldsWithType.value.value).forEach((type:any) => {
						if (!type[0].includes('.') && !defaultDataTypes.includes(type[0]) && !imports.has(type[0]) && !internalDefinedTypeName.includes(type[0]) && !definedDataNames.includes(type[0])) {
							diagnostics.push({
								severity: DiagnosticSeverity.Error,
								range: {
									start: currDocument.positionAt(fieldsWithType.value.range[0] + type[1]),
									end: currDocument.positionAt(fieldsWithType.value.range[0] + type[1] + type[0].length)
								},
								message: `Type ${type[0]} is not defined or imported`,
								source: 'Namma DSL',
							});
						}
					});
				});
		}

	});
}

function validateFieldDataTypes (){
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
	validateInternalDefinedTypes(data,imports,internalDefinedTypeName,definedDataNames);
}

function validateStorageDataFieldType (fieldType:any,imports:any,internalDefinedTypeName:any,definedDataNames:any) {
	typeSplit(fieldType.value).forEach((type:any) => {
		if (!type[0].includes('.') && !defaultDataTypes.includes(type[0]) && !imports.has(type[0]) && !internalDefinedTypeName.includes(type[0]) && !definedDataNames.includes(type[0])) {
			diagnostics.push({
				severity: DiagnosticSeverity.Error,
				range: {
					start: currDocument.positionAt(fieldType.range[0] + type[1]),
					end: currDocument.positionAt(fieldType.range[0] + type[1] + type[0].length)
				},
				message: `Type ${type[0]} is not defined or imported`,
				source: 'Namma DSL',
			});
		}
	});
}
