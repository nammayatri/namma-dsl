export let defaultDataTypes = [
		"Text",
  		"Maybe" ,
  		"Double" ,
  		"TimeOfDay" ,
  		"Day",
  		"Int" ,
  		"Bool" ,
  		"Id",
  		"Meters" ,
  		"HighPrecMeters" ,
  		"Kilometers" ,
  		"HighPrecMoney"
	]

export function typeSplit(inputType: string): [string, number][] {
    const matches: [string, number][] = [];
    let lastIndex = 0;
    const regex = /[\[\]() ]+/;
    (inputType?.split(regex) ?? []).forEach((item) => {
        if (item) {
            const startIndex = inputType.indexOf(item, lastIndex);
            matches.push([item, startIndex]);
            lastIndex = startIndex + item.length;
        }
    });
    return matches;
}