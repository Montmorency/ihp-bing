module BingView where

import Application.Helper.View (foldHSXBlocks)
import Web.View.Prelude
import Control.Lens (lens)

--Lets Make some Lens!
--lens :: (a->b) -> (a->b->a) -> Lens' a b

--View functions for handling Bing API Data in HTML
unpackBingResource :: BingResourceSet -> Html
unpackBingResource bingresource = [hsx|<div class="row justify-content-center mb-2"> 
                                               Matching addresses:
                                       </div> |] <> (renderLocationResource (resources bingresource))

renderLocationResource :: [BingLocationResource] -> Html
renderLocationResource [] = [hsx||]
renderLocationResource (binglocationresource:xs) =  [hsx|
                                <div class="card justify-content-center" style="height:24rem; width:24rem;">
                                    <div class="card-body text-center mb-2">
                                          {get #name binglocationresource} 
                                          {renderBingAddress (get #address binglocationresource)} 
                                          {(get #point binglocationresource)} 
                                          {(get #bbox binglocationresource)}
                                          {get #entityType binglocationresource}
                                          {get #confidence binglocationresource}
                                    </div>
                                </div>
                            |] <> renderLocationResource xs

renderBingPoint :: BingPoint -> Html
renderBingPoint bp = [hsx|Latitude: {lat $ coordinates bp} , Longitude: {lon $ coordinates bp}|]  
                        where 
                          lat (x:y:[]) = x 
                          lon (x:y:[]) = y 

renderBingAddress :: BingAddress -> Html
renderBingAddress addr = [hsx|
                              <div class="row justify-content-center mb-2">  
                                {(get #addressLine addr) |> fromMaybe "-"}
                              </div>
                              <div class="row justify-content-center mb-2">  
                                {(get #locality addr) |> fromMaybe "-"}
                              </div>
                              <div class="row justify-content-center mb-2">  
                                {(get #adminDistrict addr) |> fromMaybe "-"}
                              </div>
                              <div class="row justify-content-center mb-2">  
                                {(get #postalCode addr) |> fromMaybe "-"}
                              </div>
                              <div class="row justify-content-center mb-2">  
                                {(get #countryRegion addr) |> fromMaybe "-"}
                              </div>
                              |]

