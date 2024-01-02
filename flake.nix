{
  inputs.nammayatri.url = "github:nammayatri/nammayatri/ny-example";
  outputs = inputs:
    inputs.nammayatri.lib.mkNammayatriFlake { inherit inputs; }
      "ny-example";
}
